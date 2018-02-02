#!/usr/bin/env python

from __future__ import print_function

import os, random, re, sys

try:
    from cStringIO import StringIO as BytesIO
except ImportError:
    from io import BytesIO

import unittest

global VERBOSE
VERBOSE=False

import zfec

from pyutil import fileutil

from base64 import b32encode
def ab(x): # debuggery
    if len(x) >= 3:
        return "%s:%s" % (len(x), b32encode(x[-3:]),)
    elif len(x) == 2:
        return "%s:%s" % (len(x), b32encode(x[-2:]),)
    elif len(x) == 1:
        return "%s:%s" % (len(x), b32encode(x[-1:]),)
    elif len(x) == 0:
        return "%s:%s" % (len(x), "--empty--",)

def randstr(n):
    return os.urandom(n)

def _h(k, m, ss):
    encer = zfec.Encoder(k, m)
    nums_and_blocks = list(enumerate(encer.encode(ss)))
    assert isinstance(nums_and_blocks, list), nums_and_blocks
    assert len(nums_and_blocks) == m, (len(nums_and_blocks), m,)
    nums_and_blocks = random.sample(nums_and_blocks, k)
    blocks = [ x[1] for x in nums_and_blocks ]
    nums = [ x[0] for x in nums_and_blocks ]
    decer = zfec.Decoder(k, m)
    decoded = decer.decode(blocks, nums)
    assert len(decoded) == len(ss), (len(decoded), len(ss),)
    assert tuple([str(s) for s in decoded]) == tuple([str(s) for s in ss]), (tuple([ab(str(s)) for s in decoded]), tuple([ab(str(s)) for s in ss]),)

def _help_test_random():
    m = random.randrange(1, 257)
    k = random.randrange(1, m+1)
    l = random.randrange(0, 2**9)
    ss = [ randstr(l//k) for x in range(k) ]
    _h(k, m, ss)

def _help_test_random_with_l(l):
    m = random.randrange(1, 257)
    k = random.randrange(1, m+1)
    ss = [ randstr(l//k) for x in range(k) ]
    _h(k, m, ss)

def _h_easy(k, m, s):
    encer = zfec.easyfec.Encoder(k, m)
    nums_and_blocks = list(enumerate(encer.encode(s)))
    assert isinstance(nums_and_blocks, list), nums_and_blocks
    assert len(nums_and_blocks) == m, (len(nums_and_blocks), m,)
    nums_and_blocks = random.sample(nums_and_blocks, k)
    blocks = [ x[1] for x in nums_and_blocks ]
    nums = [ x[0] for x in nums_and_blocks ]
    decer = zfec.easyfec.Decoder(k, m)

    decodeds = decer.decode(blocks, nums, padlen=k*len(blocks[0]) - len(s))
    assert len(decodeds) == len(s), (ab(decodeds), ab(s), k, m)
    assert decodeds == s, (ab(decodeds), ab(s),)

def _help_test_random_easy():
    m = random.randrange(1, 257)
    k = random.randrange(1, m+1)
    l = random.randrange(0, 2**9)
    s = randstr(l)
    _h_easy(k, m, s)

def _help_test_random_with_l_easy(l):
    m = random.randrange(1, 257)
    k = random.randrange(1, m+1)
    s = randstr(l)
    _h_easy(k, m, s)

class ZFecTest(unittest.TestCase):
    def test_instantiate_encoder_no_args(self):
        try:
            e = zfec.Encoder()
            del e
        except TypeError:
            # Okay, so that's because we're required to pass constructor args.
            pass
        else:
            # Oops, it should have raised an exception.
            self.fail("Should have raised exception from incorrect arguments to constructor.")

    def test_instantiate_decoder_no_args(self):
        try:
            e = zfec.Decoder()
            del e
        except TypeError:
            # Okay, so that's because we're required to pass constructor args.
            pass
        else:
            # Oops, it should have raised an exception.
            self.fail("Should have raised exception from incorrect arguments to constructor.")

    def test_from_agl_c(self):
        self.assertTrue(zfec._fec.test_from_agl())

    def test_from_agl_py(self):
        e = zfec.Encoder(3, 5)
        b0 = b'\x01'*8 ; b1 = b'\x02'*8 ; b2 = b'\x03'*8
        # print "_from_py before encoding:"
        # print "b0: %s, b1: %s, b2: %s" % tuple(base64.b16encode(x) for x in [b0, b1, b2])

        b3, b4 = e.encode([b0, b1, b2], (3, 4))
        # print "after encoding:"
        # print "b3: %s, b4: %s" % tuple(base64.b16encode(x) for x in [b3, b4])

        d = zfec.Decoder(3, 5)
        r0, r1, r2 = d.decode((b2, b3, b4), (1, 2, 3))

        # print "after decoding:"
        # print "b0: %s, b1: %s" % tuple(base64.b16encode(x) for x in [b0, b1])

    def test_small(self):
        for i in range(16):
            _help_test_random_with_l(i)
        if VERBOSE:
            print("%d randomized tests pass." % (i+1))

    def test_random(self):
        for i in range(3):
            _help_test_random()
        if VERBOSE:
            print("%d randomized tests pass." % (i+1))

    def test_bad_args_construct_decoder(self):
        try:
            zfec.Decoder(-1, -1)
        except zfec.Error as e:
            assert "argument is required to be greater than or equal to 1" in str(e), e
        else:
            self.fail("Should have gotten an exception from out-of-range arguments.")

        try:
            zfec.Decoder(1, 257)
        except zfec.Error as e:
            assert "argument is required to be less than or equal to 256" in str(e), e
        else:
            self.fail("Should have gotten an exception from out-of-range arguments.")

        try:
            zfec.Decoder(3, 2)
        except zfec.Error as e:
            assert "first argument is required to be less than or equal to the second argument" in str(e), e
        else:
            self.fail("Should have gotten an exception from out-of-range arguments.")

    def test_bad_args_construct_encoder(self):
        try:
            zfec.Encoder(-1, -1)
        except zfec.Error as e:
            assert "argument is required to be greater than or equal to 1" in str(e), e
        else:
            self.fail("Should have gotten an exception from out-of-range arguments.")

        try:
            zfec.Encoder(1, 257)
        except zfec.Error as e:
            assert "argument is required to be less than or equal to 256" in str(e), e
        else:
            self.fail("Should have gotten an exception from out-of-range arguments.")

    def test_bad_args_dec(self):
        decer = zfec.Decoder(2, 4)

        try:
            decer.decode(98, []) # first argument is not a sequence
        except TypeError as e:
            assert "First argument was not a sequence" in str(e), e
        else:
            self.fail("Should have gotten TypeError for wrong type of second argument.")

        try:
            decer.decode(["a", "b", ], ["c", "d",])
        except zfec.Error as e:
            assert "Precondition violation: second argument is required to contain int" in str(e), e
        else:
            self.fail("Should have gotten zfec.Error for wrong type of second argument.")

        try:
            decer.decode(["a", "b", ], 98) # not a sequence at all
        except TypeError as e:
            assert "Second argument was not a sequence" in str(e), e
        else:
            self.fail("Should have gotten TypeError for wrong type of second argument.")

class EasyFecTest(unittest.TestCase):
    def test_small(self):
        for i in range(16):
            _help_test_random_with_l_easy(i)
        if VERBOSE:
            print("%d randomized tests pass." % (i+1))

    def test_random(self):
        for i in range(3):
            _help_test_random_easy()
        if VERBOSE:
            print("%d randomized tests pass." % (i+1))

    def test_bad_args_dec(self):
        decer = zfec.easyfec.Decoder(2, 4)

        try:
            decer.decode(98, [0, 1], 0) # first argument is not a sequence
        except TypeError as e:
            assert "First argument was not a sequence" in str(e), e
        else:
            self.fail("Should have gotten TypeError for wrong type of second argument.")

        try:
            decer.decode("ab", ["c", "d",], 0)
        except zfec.Error as e:
            assert "Precondition violation: second argument is required to contain int" in str(e), e
        else:
            self.fail("Should have gotten zfec.Error for wrong type of second argument.")

        try:
            decer.decode("ab", 98, 0) # not a sequence at all
        except TypeError as e:
            assert "Second argument was not a sequence" in str(e), e
        else:
            self.fail("Should have gotten TypeError for wrong type of second argument.")

class FileFec(unittest.TestCase):
    def test_filefec_header(self):
        for m in [1, 2, 3, 5, 7, 9, 11, 17, 19, 33, 35, 65, 66, 67, 129, 130, 131, 254, 255, 256,]:
            for k in [1, 2, 3, 5, 9, 17, 33, 65, 129, 255, 256,]:
                if k >= m:
                    continue
                for pad in [0, 1, k-1,]:
                    if pad >= k:
                        continue
                    for sh in [0, 1, m-1,]:
                        if sh >= m:
                            continue
                        h = zfec.filefec._build_header(m, k, pad, sh)
                        hio = BytesIO(h)
                        (rm, rk, rpad, rsh,) = zfec.filefec._parse_header(hio)
                        assert (rm, rk, rpad, rsh,) == (m, k, pad, sh,), h

    def _help_test_filefec(self, teststr, k, m, numshs=None):
        if numshs == None:
            numshs = m

        TESTFNAME = "testfile.txt"
        PREFIX = "test"
        SUFFIX = ".fec"

        fsize = len(teststr)

        tempdir = fileutil.NamedTemporaryDirectory(cleanup=True)
        try:
            tempf = tempdir.file(TESTFNAME, 'w+b')
            tempf.write(teststr)
            tempf.flush()
            tempf.seek(0)

            # encode the file
            zfec.filefec.encode_to_files(tempf, fsize, tempdir.name, PREFIX, k, m, SUFFIX, verbose=VERBOSE)

            # select some share files
            RE=re.compile(zfec.filefec.RE_FORMAT % (PREFIX, SUFFIX,))
            fns = os.listdir(tempdir.name)
            assert len(fns) >= m, (fns, tempdir, tempdir.name,)
            sharefs = [ open(os.path.join(tempdir.name, fn), "rb") for fn in fns if RE.match(fn) ]
            for sharef in sharefs:
                tempdir.register_file(sharef)
            random.shuffle(sharefs)
            del sharefs[numshs:]

            # decode from the share files
            outf = tempdir.file('recovered-testfile.txt', 'w+b')
            zfec.filefec.decode_from_files(outf, sharefs, verbose=VERBOSE)
            outf.flush()
            outf.seek(0)
            recovereddata = outf.read()
            assert recovereddata == teststr, (ab(recovereddata), ab(teststr),)
        finally:
            tempdir.shutdown()

    def test_filefec_all_shares(self):
        return self._help_test_filefec(b"Yellow Whirled!", 3, 8)

    def test_filefec_all_shares_1_b(self):
        return self._help_test_filefec(b"Yellow Whirled!", 4, 16)

    def test_filefec_all_shares_2(self):
        return self._help_test_filefec(b"Yellow Whirled", 3, 8)

    def test_filefec_all_shares_2_b(self):
        return self._help_test_filefec(b"Yellow Whirled", 4, 16)

    def test_filefec_all_shares_3(self):
        return self._help_test_filefec(b"Yellow Whirle", 3, 8)

    def test_filefec_all_shares_3_b(self):
        return self._help_test_filefec(b"Yellow Whirle", 4, 16)

    def test_filefec_all_shares_with_padding(self, noisy=VERBOSE):
        return self._help_test_filefec(b"Yellow Whirled!A", 3, 8)

    def test_filefec_min_shares_with_padding(self, noisy=VERBOSE):
        return self._help_test_filefec(b"Yellow Whirled!A", 3, 8, numshs=3)

    def test_filefec_min_shares_with_crlf(self, noisy=VERBOSE):
        return self._help_test_filefec(b"llow Whirled!A\r\n", 3, 8, numshs=3)

    def test_filefec_min_shares_with_lf(self, noisy=VERBOSE):
        return self._help_test_filefec(b"Yellow Whirled!A\n", 3, 8, numshs=3)

    def test_filefec_min_shares_with_lflf(self, noisy=VERBOSE):
        return self._help_test_filefec(b"Yellow Whirled!A\n\n", 3, 8, numshs=3)

    def test_filefec_min_shares_with_crcrlflf(self, noisy=VERBOSE):
        return self._help_test_filefec(b"Yellow Whirled!A\r\r\n\n", 3, 8, numshs=3)

    def test_filefec_mul_chunk_size(self):
        return self._help_test_filefec(randstr(6176761), 13, 16)

class Cmdline(unittest.TestCase):
    def setUp(self):
        self.tempdir = fileutil.NamedTemporaryDirectory(cleanup=True)
        self.fo = self.tempdir.file("test.data", "w+b")
        self.fo.write(b"WHEHWHJEKWAHDLJAWDHWALKDHA")
        self.realargv = sys.argv

        self.DEFAULT_M = 8
        self.DEFAULT_K = 3
        self.RE = re.compile(zfec.filefec.RE_FORMAT % ('test.data', ".fec",))

    def tearDown(self):
        sys.argv = self.realargv

    def test_basic(self, noisy=VERBOSE):
        sys.argv = ["zfec", os.path.join(self.tempdir.name, "test.data"),]

        retcode = zfec.cmdline_zfec.main()
        assert retcode == 0, retcode

        fns = os.listdir(self.tempdir.name)
        assert len(fns) >= self.DEFAULT_M, (fns, self.DEFAULT_M, self.tempdir, self.tempdir.name,)
        sharefns = [ os.path.join(self.tempdir.name, fn) for fn in fns if self.RE.match(fn) ]
        random.shuffle(sharefns)
        del sharefns[self.DEFAULT_K:]

        sys.argv = ["zunfec",]
        sys.argv.extend(sharefns)
        sys.argv.extend(['-o', os.path.join(self.tempdir.name, 'test.data-recovered'),])

        retcode = zfec.cmdline_zunfec.main()
        assert retcode == 0, retcode
        import filecmp
        assert filecmp.cmp(os.path.join(self.tempdir.name, 'test.data'),
                           os.path.join(self.tempdir.name,
                                        'test.data-recovered'))

    def test_stdin(self, noisy=VERBOSE):
        sys.stdin = open(os.path.join(self.tempdir.name, "test.data"))
        sys.argv = ["zfec", "-"]
        retcode = zfec.cmdline_zfec.main()

        fns = os.listdir(self.tempdir.name)
        assert len(fns) >= self.DEFAULT_M, (fns, self.DEFAULT_M, self.tempdir, self.tempdir.name,)
        sharefns = [ os.path.join(self.tempdir.name, fn) for fn in fns if self.RE.match(fn) ]
        random.shuffle(sharefns)
        del sharefns[self.DEFAULT_K:]

        sys.argv = ["zunfec",]
        sys.argv.extend(sharefns)
        sys.argv.extend(['-o', os.path.join(self.tempdir.name, 'test.data-recovered'),])

        retcode = zfec.cmdline_zunfec.main()
        assert retcode == 0, retcode
        import filecmp
        assert filecmp.cmp(os.path.join(self.tempdir.name, 'test.data'),
                           os.path.join(self.tempdir.name,
                                        'test.data-recovered'))

if __name__ == "__main__":
    unittest.main()
