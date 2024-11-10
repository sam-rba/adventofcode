package main

import "testing"

func TestRockVal(t *testing.T) {
	t.Parallel()

	if rock != 1 {
		t.Fail()
	}
}

func TestPaperVal(t *testing.T) {
	t.Parallel()

	if paper != 2 {
		t.Fail()
	}
}

func TestScissorsVal(t *testing.T) {
	t.Parallel()

	if scissors != 3 {
		t.Fail()
	}
}

func TestRockBeatsScissors(t *testing.T) {
	t.Parallel()

	if !rock.beats(scissors) {
		t.Fail()
	}
}

func TestRockLosesToPaper(t *testing.T) {
	t.Parallel()

	if rock.beats(paper) {
		t.Fail()
	}
}

func TestRockNotBeatRock(t *testing.T) {
	t.Parallel()

	if rock.beats(rock) {
		t.Fail()
	}
}

func TestPaperBeatsRock(t *testing.T) {
	t.Parallel()

	if !paper.beats(rock) {
		t.Fail()
	}
}

func TestPaperLosesToScissors(t *testing.T) {
	t.Parallel()

	if paper.beats(scissors) {
		t.Fail()
	}
}

func TestPaperNotBeatPaper(t *testing.T) {
	t.Parallel()

	if paper.beats(paper) {
		t.Fail()
	}
}

func TestScissorsBeatsPaper(t *testing.T) {
	t.Parallel()

	if !scissors.beats(paper) {
		t.Fail()
	}
}

func TestScissorsLosesToRock(t *testing.T) {
	t.Parallel()

	if scissors.beats(rock) {
		t.Fail()
	}
}

func TestScissorsNotBeatScissors(t *testing.T) {
	t.Parallel()

	if scissors.beats(scissors) {
		t.Fail()
	}
}

func TestLossVal(t *testing.T) {
	t.Parallel()

	if loss != 0 {
		t.Fail()
	}
}

func TestDrawVal(t *testing.T) {
	t.Parallel()

	if draw != 3 {
		t.Fail()
	}
}

func TestWinVal(t *testing.T) {
	t.Parallel()

	if win != 6 {
		t.Fail()
	}
}

func TestRockPaperLoss(t *testing.T) {
	t.Parallel()
	testRoundOutcome(rock, paper, loss, t)
}

func TestRockRockDraw(t *testing.T) {
	t.Parallel()
	testRoundOutcome(rock, rock, draw, t)
}

func TestRockScissorsWin(t *testing.T) {
	t.Parallel()
	testRoundOutcome(rock, scissors, win, t)
}

func TestPaperScissorsLoss(t *testing.T) {
	t.Parallel()
	testRoundOutcome(paper, scissors, loss, t)
}

func TestPaperPaperDraw(t *testing.T) {
	t.Parallel()
	testRoundOutcome(paper, paper, draw, t)
}

func TestPaperRockWin(t *testing.T) {
	t.Parallel()
	testRoundOutcome(paper, rock, win, t)
}

func TestScissorsRockLoss(t *testing.T) {
	t.Parallel()
	testRoundOutcome(scissors, rock, loss, t)
}

func TestScissorsScissorsDraw(t *testing.T) {
	t.Parallel()
	testRoundOutcome(scissors, scissors, draw, t)
}

func TestScissorsPaperWin(t *testing.T) {
	t.Parallel()
	testRoundOutcome(scissors, paper, win, t)
}

func testRoundOutcome(you, opponent Hand, want Outcome, t *testing.T) {
	round := Round{you, opponent}
	got := round.outcome()
	if got != want {
		t.Errorf("outcome of %v is %v, want %v", round, got, want)
	}
}

func TestWinRockScore(t *testing.T) {
	t.Parallel()
	testRoundScore(rock, scissors, 6+1, t)
}

func TestWinPaperScore(t *testing.T) {
	t.Parallel()
	testRoundScore(paper, rock, 6+2, t)
}

func TestWinScissorsScore(t *testing.T) {
	t.Parallel()
	testRoundScore(scissors, paper, 6+3, t)
}

func TestDrawRockScore(t *testing.T) {
	t.Parallel()
	testRoundScore(rock, rock, 3+1, t)
}

func TestDrawPaperScore(t *testing.T) {
	t.Parallel()
	testRoundScore(paper, paper, 3+2, t)
}

func TestDrawScissorsScore(t *testing.T) {
	t.Parallel()
	testRoundScore(scissors, scissors, 3+3, t)
}

func TestLossRockScore(t *testing.T) {
	t.Parallel()
	testRoundScore(rock, paper, 0+1, t)
}

func TestLossPaperScore(t *testing.T) {
	t.Parallel()
	testRoundScore(paper, scissors, 0+2, t)
}

func TestLossScissorsScore(t *testing.T) {
	t.Parallel()
	testRoundScore(scissors, rock, 0+3, t)
}

func testRoundScore(you, opponent Hand, want int, t *testing.T) {
	round := Round{you, opponent}
	got := round.score()
	if got != want {
		t.Errorf("score of %v is %d, want %d", round, got, want)
	}
}
