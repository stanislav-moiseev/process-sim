# `process-sim`, a simulation environment for communicating processes

## How to install

 1. Install [stack](https://haskellstack.org/).

 2. Clone the repository and initialize the `stack` project:

    ```
    git clone https://github.com/stanislav-moiseev/process-sim
    cd process-sim/
    stack init
    ```

 3. Build the project:

    ```
    stack build
    ```

 4. Run tests:

    ```
    stack exec test1 [number-of-processes]
    stack exec test2 [number-of-processes]
    ```
