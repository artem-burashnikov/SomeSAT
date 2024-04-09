# SomeSAT

[![Build status][status-shield]][status-url]
[![MIT License][license-shield]][license-url]

## Overview

SAT solver which utilizes DPLL search algorithm.

## Getting Started

### Prerequisites

**For running**:
- [dotnet SDK 8.0](https://dotnet.microsoft.com/en-us/download/dotnet/8.0)

**For testing**:
- [Python 3.10+](https://wiki.python.org/moin/BeginnersGuide/Download)
- picosat

### Installation

Open the terminal and follow these steps:

1. Clone the repository:

    ```shell
    git clone git@github.com:artem-burashnikov/SomeSAT.git
    ```
2. Navigate to the project folder inside the root:
   ```shell
   cd SomeSAT/
   ```
3. Build the project:
   ```shell
   dotnet build --configuration Release
   ```

4. Run the solver against a file in DIMACS format:
   ```shell
   dotnet run <filePath>
   ```

## License

The project is licensed under a [MIT-License][license-url].

<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[license-shield]: https://img.shields.io/github/license/artem-burashnikov/SomeSAT.svg?style=for-the-badge&color=blue
[status-shield]: https://img.shields.io/github/actions/workflow/status/artem-burashnikov/SomeSAT/.github/workflows/ci.yml?branch=main&event=push&style=for-the-badge
[status-url]: https://github.com/artem-burashnikov/SomeSAT/blob/main/.github/workflows/ci.yml
[license-url]: LICENSE
