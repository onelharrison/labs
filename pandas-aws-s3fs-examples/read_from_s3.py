"""
Script demonstrating how to read a csv file from Amazon S3
into a pandas data frame using s3fs support in pandas
"""

import os

import pandas as pd

AWS_S3_BUCKET = os.getenv("AWS_S3_BUCKET")
AWS_ACCESS_KEY_ID = os.getenv("AWS_ACCESS_KEY_ID")
AWS_SECRET_ACCESS_KEY = os.getenv("AWS_SECRET_ACCESS_KEY")
AWS_SESSION_TOKEN = os.getenv("AWS_SESSION_TOKEN")

key = "files/books.csv"

books_df = pd.read_csv(
    f"s3://{AWS_S3_BUCKET}/{key}",
    storage_options={
        "key": AWS_ACCESS_KEY_ID,
        "secret": AWS_SECRET_ACCESS_KEY,
        "token": AWS_SESSION_TOKEN,
    },
)

print(books_df)
