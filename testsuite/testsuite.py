#! /usr/bin/env python

import sys

from e3.testsuite import Testsuite
from e3.testsuite.driver.diff import DiffTestDriver


def valgrind_wrap(driver, argv):
    """
    When running the testsuite in valgrind mode, wrap the given command line
    arguments so that the program is run under Valgrind. Return them unchanged
    otherwise.
    """
    result = list(argv)
    if driver.env.options.valgrind:
        result = ["valgrind", "-q", "--leak-check=full"] + result
    return result


class CompileAndRunDriver(DiffTestDriver):
    """Driver"""
    def run(self):
        project_file = self.test_env.get("project_file", None)
        executable = self.test_env.get("executable", ["./test"])
        args_matrix = self.test_env.get("args_matrix", [[]])

        build_command = ["gprbuild"]
        if project_file is not None:
            build_command += ["-P", project_file]

        self.shell(build_command, analyze_output=False)

        for exec_args in args_matrix:
            if exec_args:
                self.output += "[" + " ".join(exec_args) + "]\n"
            run_command = ["./" + executable] + exec_args
            self.shell(valgrind_wrap(self, run_command), analyze_output=True)


class AdaSATTestsuite(Testsuite):
    """Testsuite for AdaSAT."""
    test_driver_map = {"compile_and_run": CompileAndRunDriver}
    default_driver = "compile_and_run"

    def add_options(self, parser):
        parser.add_argument(
            "--rewrite", action="store_true",
            help="Rewrite test baselines according to current outputs"
        )
        parser.add_argument(
            "--valgrind", action="store_true",
            help="Run tests within Valgrind to check memory issues."
        )

    def set_up(self):
        super(AdaSATTestsuite, self).set_up()
        self.env.rewrite_baselines = self.main.args.rewrite


if __name__ == "__main__":
    sys.exit(AdaSATTestsuite().testsuite_main())
