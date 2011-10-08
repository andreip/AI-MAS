import subprocess
import time
import os

def WriteStats(out, timeout, runtime):
	if runtime == -1:
		actual = "stopped (maxtime exceeded)"
	else:
		actual = runtime
	
	out.write("{0:<10} ...: {1:>6.3f}\n".format("MaxTime", timeout) +
	          "{0:<10} ...: {1:>6.3f}\n".format("ActualTime", actual))
	out.flush()

def run(command, out, err, timeout):
	proc = subprocess.Popen(command, bufsize=0, stdout=out, stderr=err)
	poll_seconds = .25
	start = time.time()
	deadline = start + timeout

	while time.time() < deadline and proc.poll() == None:
		time.sleep(poll_seconds)

	if proc.poll() == None:
		proc.terminate()
		runtime = -1
	else:
		runtime = time.time() - start
	WriteStats(out, timeout, runtime)

	return runtime

