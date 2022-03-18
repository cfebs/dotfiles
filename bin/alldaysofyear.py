#!/usr/bin/env python3

import sys
import argparse
from datetime import date, timedelta

parser = argparse.ArgumentParser(description='Generate every weekday of choice for a year.')
parser.add_argument('year', type=int)
parser.add_argument('weekday', type=int)
args = parser.parse_args()

def alldays(year, weekday):
   d = date(year, 1, 1)
   while d.weekday() != weekday:
       d += timedelta(days = 1)

   while d.year == year:
      yield d
      d += timedelta(days = 7)

for d in alldays(args.year, args.weekday):
   print(d)
