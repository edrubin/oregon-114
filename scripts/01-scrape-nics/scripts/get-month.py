#!/usr/bin/env python
import datetime
import re
from io import BytesIO

import pdfplumber
import requests


def parse_date(pdf):
    text = pdf.pages[0].extract_text(x_tolerance=5)
    date_pat = r"UPDATED:\s+As of (.+)\n"
    updated_date = re.search(date_pat, text).group(1)
    # Fixing strange date formatting
    updated_date = updated_date.replace(" , ", ", ")
    d = datetime.datetime.strptime(updated_date, "%B %d, %Y")
    return d


if __name__ == "__main__":
    URL = "https://www.fbi.gov/file-repository/active_records_in_the_nics-index.pdf"
    raw = requests.get(URL).content
    # pdf = pdfplumber.load(BytesIO(raw))
    pdf = pdfplumber.open(BytesIO(raw))
    d = parse_date(pdf)
    print(d.strftime("%Y-%m"))
