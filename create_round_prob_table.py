import math
import pandas as pd


def _integer_or_bounds(value: float) -> tuple[int, ...]:
    """Return the integer vote counts that *could* be reported for a
    floating-point expectation.

    If the value is already an integer -> (value,)
    Otherwise -> (floor(value), ceil(value))
    """
    return (int(value),) if value.is_integer() else (math.floor(value), math.ceil(value))


def probability_roundness(voters_number: int) -> dict[str, float]:
    """Compute probabilities for hitting a *round* percentage of the winner's votes
    in a precinct with *voters_number* ballots cast.

    Two grids are supported:
    **p_005** - 5-percentage-point grid: 5 %, 10 %, ..., 100 %
    **p_001** - 1-percentage-point grid: 1 %, 2 %, ..., 100 %

   
    The probability is the count of *distinct* integer vote totals that would
    produce any of the round percentages, divided by *voters_number*.
    """
    probabilities: dict[str, float] = {}

    # --- 5-percentage-point grid -------------------------------------------
    round_counts_5 = set()
    for pct in range(5, 101, 5):  # 5 %, 10 %, ..., 100 %
        round_counts_5.update(_integer_or_bounds(voters_number * pct / 100))
    probabilities["p_005"] = len(round_counts_5) / voters_number

    # --- 1-percentage-point grid -------------------------------------------
    round_counts_1 = set()
    for pct in range(1, 101):  # 1 %, 2 %, ..., 100 %
        round_counts_1.update(_integer_or_bounds(voters_number * pct / 100))
    probabilities["p_001"] = len(round_counts_1) / voters_number

    return probabilities


def build_probability_table(min_voters: int = 100, max_voters: int = 3000) -> pd.DataFrame:
    """Create a DataFrame with roundness probabilities for every precinct size
    from *min_voters* to *max_voters* (inclusive).
    """
    records: list[dict[str, float]] = []
    for n in range(min_voters, max_voters + 1):
        record = {"voters_number": n}
        record.update(probability_roundness(n))
        records.append(record)
    return pd.DataFrame.from_records(records)


if __name__ == "__main__":
    df = build_probability_table()

    # Inspect the first few rows in the console
    print(df.head())

    # Persist for later analysis
    df.to_csv("round_probabilities_table.csv", index=False)
    # df.to_pickle("round_probabilities_1pct_5pct.pkl")  # handy for Pandas users
