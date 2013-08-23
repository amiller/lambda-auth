import re
import itertools
import numpy as np
from pylab import *

def split_bytes(s):
    return [x.split()[0] for x in s.strip().split('\n')]

def split_bytes2(s):
    return [x.split()[0] for x in s.strip().split()]

insert_bytes = """
  97600000 data/proof_rbp_ins_004.dat
 119800000 data/proof_rbp_ins_005.dat
 177400000 data/proof_rbp_ins_006.dat
 199100000 data/proof_rbp_ins_007.dat
 222300000 data/proof_rbp_ins_008.dat
 214700000 data/proof_rbp_ins_009.dat
 344100000 data/proof_rbp_ins_010.dat
 324200000 data/proof_rbp_ins_011.dat
 316900000 data/proof_rbp_ins_012.dat
 454600000 data/proof_rbp_ins_013.dat
 272800000 data/proof_rbp_ins_014.dat
 411800000 data/proof_rbp_ins_015.dat
 446300000 data/proof_rbp_ins_016.dat
 433700000 data/proof_rbp_ins_017.dat
 504800000 data/proof_rbp_ins_018.dat
 484800000 data/proof_rbp_ins_019.dat
 526600000 data/proof_rbp_ins_020.dat
 616200000 data/proof_rbp_ins_021.dat
"""

lookup_bytes_1 = {"susp":""" 461339 791075 1608942 3469317 6160326 9550276 13224785 16696347 20044648 23296408 26659471 29881210 33027057 36311268 39435931 42457267 45590807 48675253
""",
"plain":"""515878 349613 1846633 4358128 8449247 13757028 19789451 25734593 31508284 37106293 42940869 48592397 54208447 59850566 65367626 70652605 76050585 81210401""",
"buf":"""1206 2225 4288 8364 16506 33546 3860339 10425933 17639100 24242514 30886116 37015976 42998173 48976241 54675138 60360493 65959912 71558228"""}

loookup_bytes_5 = {"plain":"""592338 739751 1034053 1287490 1448282 1447699 2248572 2088417 2088422 3129866 1927677 2956326 3303386 3129863 3651087 3476937 """,}

ins_prover_path = "data/results-rbpins-prover4.txt"
ins_verifier_path = "data/results-rbpins-verifier4.txt"
ins_ideal_path = "data/results-rbpins-ideal4.txt"

#mtree1_prover_path = "data/results-mtree1-prover.txt"
mtree1_verifier_path = "data/results-mtree1-verifier4.txt"
mtree_verifier_path = "data/results-mtree-verifier4.txt"
cmerkle_verifier_path = "data/results-cmerkle4.txt"

look_prover_susp_path = "data/result-rbplook-prover-susp.txt"

def find_alloc(path):
    lines = open(path).readlines()
    def parse():
        for line in lines:
            m = re.findall("Allocated bytes: (\d+)", line)
            if m: yield float(m[0])
    p = list(parse())
    return p

def find_result(path):
    lines = open(path).readlines()
    def parse():
        for line in lines:
            m = re.findall("x(\d+).+2\^([\d\.]+)", line)
            if m: iters = int(m[0][0]); k = int(m[0][1])
            m = re.findall("([\d\.]+)/s.+n=(\d+)", line)
            if m: yield k, 1/float(m[0][0])
    p = parse()

    # Group by the number of operations
    p1 = itertools.groupby(p, lambda x: x[0])

    # Convert to list, get in the form [[k, [dk0,dk1...dkn]], ...]
    p2 = [[p[0],[pp[1] for pp in p[1]]] for p in p1]

    # Find the percentiles
    p3 = [[p[0], np.percentile(p[1],25), np.percentile(p[1],50), np.percentile(p[1],75)] for p in p2];
    return np.array(p3)

def find_cmerkle(path):
    lines = open(path).readlines()
    def parse():
        for line in lines:
            m = re.findall("2\^(\d+).+x(\d+).+ ([\d\.]+) seconds", line)
            if m: yield int(m[0][0]), float(m[0][2])
    p = parse()
    p1 = itertools.groupby(p, lambda x: x[0])
    p2 = [[p[0],[pp[1] for pp in p[1]]] for p in p1]
    p3 = [[p[0], np.percentile(p[1],25), np.percentile(p[1],50), np.percentile(p[1],75)] for p in p2];
    return np.array(p3)


def graphs2():
    f = figure(0)
    clf()
    dat = find_result(ins_verifier_path)
    errorbar(dat[:,0], dat[:,2], yerr=[dat[:,2]-dat[:,1],dat[:,3]-dat[:,2]], fmt='go-', label='verifier')
    dat = find_result(ins_ideal_path)
    errorbar(dat[:,0], dat[:,2], yerr=[dat[:,2]-dat[:,1],dat[:,3]-dat[:,2]], fmt='ko-', label='ideal')
    dat = find_result(ins_prover_path)
    errorbar(dat[:,0], dat[:,2], yerr=[dat[:,2]-dat[:,1],dat[:,3]-dat[:,2]], fmt='ro-', label='prover')
    show()
    legend(loc='upper left')
    title('Running time (insert x10000) into RB+) (median, 5 trials, 25%/75% error bars)')
    xlabel('$\log_2$ tree size')
    ylim(ymin=0)
    ylabel('running time (s)')

    f = figure(3,(5.0,5.0))
    clf()
    di = find_result(ins_ideal_path)
    dp = find_result(ins_prover_path)
    dv = find_result(ins_verifier_path)
    plot(di[:,0], di[:,2],'ko-', markersize=5)
    plot(dp[:,0], dp[:,2],'r+-.', markersize=9)
    plot(dv[:,0], dv[:,2],'gx-', markersize=9)
    legend(['Ideal','Prover','Verifier'], loc='upper left')
    #title('Running time (insert x100000) into RB+) (median, 20 trials)')
    xlabel('$\log_2$ tree size')
    yscale('log')
    ylim(ymax=200)
    xlim(xmax=21)
    ylabel('running time for 100,000 inserts (s)')
    show()
    f.savefig("../../graphics/rbp_runtime.pdf")

    f = figure(7,(4.5,4.5))
    clf()
    dv = find_result(mtree_verifier_path)
    dv1 = find_result(mtree1_verifier_path)
    global dvc
    dvc = find_cmerkle(cmerkle_verifier_path)
    def erbar(dat, fmt, label):
        errorbar(dat[:,0], dat[:,2], yerr=[dat[:,2]-dat[:,1],dat[:,3]-dat[:,2]], 
                 fmt=fmt, label=label)
    plot(dv[:,0]+1, dv[:,2],'gx-',)
    plot(dv1[:,0]+1, dv1[:,2],'b^--')
    plot(dvc[:,0]+1, dvc[:,2],'mv-.')
    legend(['$\lambda \\bullet$','ocaml','C'], loc='upper left')
    #erbar(dv, 'g^-', '$\lambda \\bullet$')
    #erbar(dv1, 'bo-', 'ocaml')
    #erbar(dvc, 'mx-', 'C')
    legend(loc='upper left')
    #title('Verifier running time (Merkle tree lookup, x100000) (median, 5 trials)')
    xlabel('tree height')
    #xlim(xmin=4,xmax=20)
    ylim(ymin=0)
    ylabel('running time for 100,000 queries (s)')
    show()
    f.savefig("../../graphics/vshandrolled.pdf")

    f = figure(5,(5.0,5.0))
    clf()
    ap = np.array(find_alloc(ins_prover_path))
    av = np.array(find_alloc(ins_verifier_path))
    ai = np.array(find_alloc(ins_ideal_path))
    print ap
    # Measurements are in words, n*4 for 32 bit words
    plot(range(4,22), ai*4, 'ko-')
    plot(range(4,22), ap*4, 'r+-.')
    plot(range(4,22), av*4, 'gx-')
    yscale('log')
    #ylim(ymin=1)
    legend(['Ideal','Prover','Verifier'], loc='upper left')
    #title('Memory usage (insert x100000) into RB+')
    xlabel('$log_2$ tree size')
    ylabel('heap allocation (bytes)')
    show()
    f.savefig("../../graphics/memoryusage.pdf")
    

    figure(4)
    clf()
    di = find_result(ins_ideal_path)
    dp = find_result(ins_prover_path)
    plot(di[:,0], dp[:,2]/di[:,2])
    ylim(ymin=0)

    f = figure(6,(4.5,4.5))
    clf()
    global plain, susp
    dat = split_bytes2(lookup_bytes_1['plain'])
    plot(range(4,22), [float(d)/1000000 for d in dat], 'gx-')
    plain = np.array(map(float,dat))
    dat = split_bytes2(lookup_bytes_1['susp'])
    plot(range(4,22), [float(d)/1000000 for d in dat], 'ms-')
    susp = np.array(map(float,dat))
    dat = split_bytes2(lookup_bytes_1['buf'])
    plot(range(4,22), [float(d)/1000000 for d in dat], 'D-.')
    legend(('original','susp-disbelief','reuse (1000)'), loc='upper left')
    #title('Proof size (after gzip), operation x10000')
    #yscale('log')
    show()
    xlabel('$\log_2$ tree size')
    ylabel('proof size for 100,000 queries (MB)')
    f.savefig("../../graphics/proofsize.pdf")
