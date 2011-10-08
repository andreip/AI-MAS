#!/usr/bin/env python2.6

import os
import subprocess
import sys
from test.python import proc


def cleanup(name):
	if os.path.exists(name):
		os.remove(name)


def OpenFile(name):
	cleanup(name)
	f = open(name, 'w+')
	return f


def WriteToFile(File, Msg):
	File.write(Msg)
	File.flush()


def WriteCompilationMessages():
	subprocess.call(["swipl", "-g", "[test],halt."])


def Add(lista):
	suma = 0
	for i in lista:
		suma += i
	return suma


def Parse():
	'''Parse tema1.plt to get things like
	timeout, name, points etc.'''

	test = os.path.abspath('test')
	f = open(test + os.sep + "tema1.plt", "r")
	
	# get the test-unit name
	for line in f:
		if line.find("begin_tests") > -1:
			about = (line.split("("))[1].split(')')
			test_name = about[0]
			break

	tests = []
	timeouts = []
	punctaje = []
	for line in f:
		# get test names
		if line.find("test(") == 0:
			test = line.split("'")[1]
			tests.append(test_name + ":" + "'" + test + "'")
		# get test timeouts
		if line.find("%#") == 0:
			if line.split()[1] == "timeout":
				timeouts.append( int( line.split()[2] ) )
			elif line.split()[1] == "global":
				global_t = int( line.split()[2] )
			elif line.split()[1] == "punctaj":
				punctaje.append( int( line.split()[2] ) )

	f.close()
	return tests, timeouts, punctaje, global_t

def SearchOverall(tmp):
	tmp.seek(0, os.SEEK_SET)

	for line in tmp:
		if line.find("Overall:") >= 0:
			status = str(line.split()[1])
	return status


def WriteToTmp(tmp):
	'''Write test details and summary'''

	tests, timeouts, punctaje, global_t = Parse()
	passed = 0
	punctaj = 0
	punctaj_maxim = Add(punctaje)
	dots = "." * 30 + "\n"

	string = "Initial Global Time: " + str(global_t) + " s" + "\n"
	WriteToFile(tmp, "\n" + dots + string)

	# test details
	for i in range(0, len(tests)):
		timeout = min(timeouts[i], global_t)
		command = "swipl", "-g", "[test],(run_tests(" + tests[i] + "); true),halt."
		runtime = proc.run(command, tmp, tmp, timeout)

		if runtime == -1:
			global_t -= timeout
		else:
			global_t -= runtime

		if global_t <= 0:
			string = "{0:<10} ...: exceeded\n".format("GlobalTime")
			WriteToFile(tmp, string)
			break
		else:
			string = "{0:<10} ...: {1:>6.3f} s\n".format("GlobalTime", global_t)
			WriteToFile(tmp, string)

		if runtime == -1:
			string = "{0:<10} ...: Test Failed\n".format("Timeout")
			WriteToFile(tmp, string)
			continue

		if SearchOverall(tmp) == 'ok':
			passed += 1
			punctaj += punctaje[i]

	# testing summarys
	summary = "SUMMARY:\n"
	passed = "\t{0:>3}/{1:<3} tests passed\n".format(passed, len(tests))
	points = "\t{0:>3}/{1:<3} points\n".format(punctaj, punctaj_maxim)
	WriteToFile(tmp, "\n" + dots + summary + passed + points + dots)


def FilterTmpWriteToOutput(tmp):
	'''Filter the output, based on the tmp file'''

	tmp.seek(0, os.SEEK_SET)

	for line in tmp:
		if line.find('%') != 0:
			sys.stdout.write(line)


if __name__ == '__main__':
	'''stdout contains the output of all 
	tests and also the compilation message'''

	tmp_name = 'tmp.txt'
	tmp = OpenFile(tmp_name)

	WriteCompilationMessages()
	WriteToTmp(tmp)
	FilterTmpWriteToOutput(tmp)

	tmp.close()
	cleanup(tmp_name)
