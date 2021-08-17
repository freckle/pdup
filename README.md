# PDUp

Report outage minutes and calculate uptime based on PagerDuty incidents.

## Features

- Handles Retry (on 429) and pagination of PagerDuty API
- Accepts days or absolute time range (see usage)
- Combines overlapping Incidents (in other words, you can only be down 1 minute
  in any given minute)

## Installation

```
git clone https://github.com/freckle/pdup
cd pdup
stack build --fast --pedantic
```

Create `.env`:

```
PAGERDUTY_TOKEN={Your PagerDuty API token}
```

## Usage

```console
% stack exec pdup -- --help
Usage: pdup [(-t|--team ARG) [-t|--team ARG]] [-S|--ignore-service ARG] 
            ((-d|--days N) | (-s|--since YYYY-MM-DD) [-u|--until YYYY-MM-DD])

Available options:
  -t,--team ARG            Filter by Team Id
  -t,--team ARG            Filter by Team Id
  -S,--ignore-service ARG  Ignore Service Id
  -d,--days N              Query over previus N days from now
  -s,--since YYYY-MM-DD    Query since start of date
  -u,--until YYYY-MM-DD    Query until end of
                           date (default: 2021-08-17 18:50:14.662479083 UTC)
  -h,--help                Show this help text
```

To show information for the previous 7, 30, and 90 days, run `bin/report`.

---

[LICENSE](./LICENSE) | [CHANGELOG](./CHANGELOG.md)
