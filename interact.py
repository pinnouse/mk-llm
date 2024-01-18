"""Interaction module heavily based off
https://github.com/xuexue/neuralkanren/blob/master/interact.py"""

from __future__ import print_function
import random
from subprocess import Popen, PIPE
from time import sleep

class Interaction(object):
    """Interaction object communicates with racket to solve the miniKanren
    Programming by Example query.

    Usage:
        with Interaction(query) as env:
            ...
    """

    MK_SCRIPT = 'interact.rkt'
    PROMPT_ENDING = '>'

    def __init__(self, query):
        self.query = query
        self.proc = None
        self.state = None
        self.good_path = None

    def __enter__(self):
        """Start racket process, send query to process."""
        self.proc = Popen(['racket', self.MK_SCRIPT], bufsize=0, text=True, stdin=PIPE, stdout=PIPE)
        # TODO: Send an actual query

        # self._send(self.query)
        # self._read_state()
        return self

    def __exit__(self, *args):
        """Stop the process."""
        if self.proc is not None:
            # self.proc.stdin.close()
            self.proc.terminate();
            self.proc.wait(timeout=3);

    def _read(self):
        """Helper function to read from process."""
        txt = self.proc.stdout.readline().rstrip()
        if not txt:
            return None
        return txt

    def _send(self, datum):
        """Helper function to write to the racket process.

        Args:
            datum: the content of the message to be relayed in plain text.
        """
        self.proc.stdin.write(datum + '\n')
        self.proc.stdin.flush()

    def _good_path(self):
        """Populate self.good_path by interacting with the process."""
        if self.state is None:
            self.good_path = None
        else:
            self._send('good-path')
            self.good_path = self._read()

    def _read_state(self):
        """Populate self.state and self.good_path by interacting with the process."""
        self.state = self._read()
        self._good_path()

    def read_prompt(self):
        """Helper to try and read the whole given prompt.
        """
        buff = ""
        nextline = self._read()
        # while nextline is not None:
        while True:
            if nextline is not None:
                buff += nextline
                if self.PROMPT_ENDING in nextline \
                   or 'Hit enter to continue' in nextline:
                    break
            buff += '\n'
            nextline = self._read()
        return buff

    def acceptable_input(self, datum: str):
        if datum in ['h', 'u']:
            return True
        elif datum.isdecimal():
            return True
        return False

    def send(self, datum: str):
        datum = datum.strip()
        if not self.acceptable_input(datum):
            return
        self._send(datum)

    def follow_path(self, path: str):
        """Communicate to racket process to expand candidate at given path.

        Args:
            path: choice to follow
        """
        self._send(path)
        feedback = self._read()
        self._read_state()
        return feedback

    def steps_remaining(self):
        self._send('steps-remaining')
        return self._read()

    def jump_to_steps_remaining(self, n):
        # TODO: Determine if these extra commands are required.
        self._send(['jump-to-steps-remaining', n])
        self._read_state()

