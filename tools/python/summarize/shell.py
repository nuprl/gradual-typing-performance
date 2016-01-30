"""
Wrapper for executing system commands and returning the result as a string.
"""

import subprocess

def execute(command, on_failure=None):
    """ (-> String (-> CalledProcessException String) String)
        Run `command` string as a shell subprocess,
        return data printed to STDOUT.
        Optional argument `on_failure` used to handle internal errors.
        If the subprocess fails, `on_failure` is called with the resulting
        exception.
    """
    output = None
    # Try to run the command, watch for failures
    # Need to close pipe in a finally block? I think not but leaving note just.in.case
    try:
        output = subprocess.check_output(command, shell=True, stderr=subprocess.STDOUT)
    except subprocess.CalledProcessError as cpe:
        if on_failure is None:
            raise cpe
        else:
            return on_failure(cpe)
    # Success! Convert the output to string
    if not isinstance(output, str):
        # 2013-09-07: Explicitly convert bytestring to str, for python3
        output = output.decode('utf-8')
    return output

def find_file(filename):
    command = " ".join(["find"
                        ,"."
                        ,"-name"
                        ,filename])
    matches_str = execute(command)
    if matches_str:
        return matches_str.split("\n",1)[0]
    return None
