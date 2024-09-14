package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
	"strings"
)

var (
	ruleExpr = regexp.MustCompile("[xmas](<|>)[0-9]+(:)[[:alpha:]]+")
	partExpr = regexp.MustCompile("\\{x=([0-9]+),m=([0-9]+),a=([0-9]+),s=([0-9]+)\\}")
)

type Part struct {
	x, m, a, s uint64
}

type Rule struct {
	accept func(Part) bool
	dst    string
}

type Workflow struct {
	name  string
	rules []Rule
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)

	workflows, err := parseWorkflows(scanner)
	if err != nil {
		log.Fatal(err)
	}

	parts, err := parseParts(scanner)
	if err != nil {
		log.Fatal(err)
	}

	var sum uint64 = 0
	for _, part := range accepted(parts, workflows) {
		sum += part.x + part.m + part.a + part.s
	}
	fmt.Println("silver:", sum)
}

func parseWorkflows(sc *bufio.Scanner) (map[string]Workflow, error) {
	workflows := make(map[string]Workflow)

	for sc.Scan() {
		line := sc.Text()
		if len(line) <= 0 {
			break
		}
		wf, err := parseWorkflow(line)
		if err != nil {
			return nil, err
		}
		workflows[wf.name] = wf
	}
	return workflows, nil
}

func parseWorkflow(str string) (Workflow, error) {
	var wf Workflow

	i := strings.IndexByte(str, '{')
	if i < 1 {
		return Workflow{}, ErrInvalidWorkflow{str}
	}
	var rest string
	wf.name, rest = str[:i], str[i+1:len(str)-1]

	for _, s := range strings.Split(rest, ",") {
		rule, err := parseRule(s)
		if err != nil {
			return Workflow{}, err
		}
		wf.rules = append(wf.rules, rule)
	}
	return wf, nil
}

func parseRule(str string) (Rule, error) {
	matches := ruleExpr.FindStringSubmatchIndex(str)

	if matches == nil {
		return Rule{
			func(p Part) bool { return true },
			str,
		}, nil
	}

	v, err := strconv.ParseUint(str[matches[2*1]+1:matches[2*2]], 10, 64)
	if err != nil {
		return Rule{}, err
	}

	var cmp func(category uint64) bool
	switch str[matches[2*1]:matches[2*1+1]] {
	case "<":
		cmp = func(category uint64) bool { return category < v }
	case ">":
		cmp = func(category uint64) bool { return category > v }
	default:
		return Rule{}, ErrInvalidRule{str + " (invalid comparator)"}
	}

	category := str[:matches[2*1]]
	dst := str[matches[2*2]+1:]
	var accept func(p Part) bool
	switch category {
	case "x":
		accept = func(p Part) bool { return cmp(p.x) }
	case "m":
		accept = func(p Part) bool { return cmp(p.m) }
	case "a":
		accept = func(p Part) bool { return cmp(p.a) }
	case "s":
		accept = func(p Part) bool { return cmp(p.s) }
	default:
		return Rule{}, ErrInvalidRule{str + " (invalid category)"}
	}

	return Rule{accept, dst}, nil
}

func parseParts(sc *bufio.Scanner) ([]Part, error) {
	var parts []Part

	for sc.Scan() {
		line := sc.Text()
		part, err := parsePart(line)
		if err != nil {
			return nil, err
		}
		parts = append(parts, part)
	}
	return parts, nil
}

func parsePart(str string) (Part, error) {
	matches := partExpr.FindStringSubmatch(str)
	if len(matches) < 5 {
		return Part{}, ErrInvalidPart{str}
	}
	categories := make([]uint64, 4)
	var err error
	for i := range categories {
		categories[i], err = strconv.ParseUint(matches[i+1], 10, 64)
		if err != nil {
			return Part{}, ErrInvalidPart{str}
		}
	}
	return Part{categories[0], categories[1], categories[2], categories[3]}, nil
}

func accepted(parts []Part, workflows map[string]Workflow) []Part {
	accept := make([]Part, 0, len(parts))
	for _, part := range parts {
		workflow := "in"
		for workflow != "A" && workflow != "R" {
			workflow = destination(part, workflows[workflow])
		}
		if workflow == "A" {
			accept = append(accept, part)
		}
	}
	return accept
}

func destination(part Part, workflow Workflow) string {
	for _, rule := range workflow.rules {
		if rule.accept(part) {
			return rule.dst
		}
	}
	panic("unreachable")
}

type ErrInvalidWorkflow struct {
	str string
}

func (e ErrInvalidWorkflow) Error() string {
	return fmt.Sprintf("invalid workflow: %s", e.str)
}

type ErrInvalidRule struct {
	str string
}

func (e ErrInvalidRule) Error() string {
	return fmt.Sprintf("invalid rule: %s", e.str)
}

type ErrInvalidPart struct {
	str string
}

func (e ErrInvalidPart) Error() string {
	return fmt.Sprintf("invalid part: %s", e.str)
}
