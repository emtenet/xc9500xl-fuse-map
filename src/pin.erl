-module(pin).

-export_type([pin/0]).

-type pin() ::
    a2 | a3 | a4 | a5 | a6 | a7 | a8 | a9 | a10 |
    a11 | a12 | a13 | a14 | a15 | a16 | a17 | a18 |
    b1 | b2 | b3 | b4 | b5 | b6 | b7 | b8 | b9 | b10 |
    b11 | b12 | b13 | b14 | b15 | b16 | b17 | b18 | b19 |
    c1 | c2 | c3 | c4 | c5 | c6 | c7 | c8 | c9 | c10 |
    c11 | c12 | c13 | c14 | c15 | c16 | c17 | c18 | c20 |
    d1 | d2 | d3 | d4 | d5 | d6 | d7 | d8 | d9 | d10 |
    d11 | d12 | d13 | d14 | d15 | d16 | d17 | d18 | d19 | d20 |
    e1 | e2 | e3 | e4 | e5 | e6 | e7 | e8 | e9 | e10 |
    e11 | e12 | e13 | e14 | e15 | e16 | e17 | e18 | e19 | e20 |
    f1 | f2 | f3 | f4 | f5 | f6 | f10 |
    f11 | f12 | f13 | f14 | f15 | f16 | f17 | f18 | f19 |
    g1 | g2 | g3 | g4 | g5 | g6 | g7 | g10 |
    g11 | g12 | g14 | g15 | g16 | g17 | g19 | g20 |
    h1 | h2 | h3 | h4 | h5 | h10 |
    h11 | h12 | h13 | h14 | h15 | h16 | h17 | h18 | h19 | h20 |
    j1 | j2 | j3 | j4 | j5 | j10 |
    j11 | j12 | j13 | j14 | j15 | j16 | j17 | j18 | j19 | j20 |
    k1 | k2 | k3 | k4 | k5 | k6 | k7 | k8 | k9 | k10 |
    k11 | k12 | k13 | k14 | k15 | k16 | k17 | k18 | k19 |
    l1 | l2 | l3 | l4 | l5 | l6 | l8 |
    l11 | l12 | l13 | l15 | l16 | l17 | l18 | l19 | l20 |
    m1 | m2 | m3 | m4 | m5 | m6 | m7 | m8 | m9 | m10 |
    m11 | m12 | m13 | m14 | m15 | m16 | m17 | m18 | m19 | m20 |
    n1 | n2 | n3 | n4 | n5 | n6 | n7 | n8 | n9 | n10 |
    n11 | n12 | n13 | n14 | n15 | n16 | n17 | n18 | n19 | n20 |
    p1 | p2 | p3 | p4 | p5 | p6 | p7 | p8 | p9 | p10 |
    p11 | p12 | p13 | p14 | p15 | p16 | p17 | p18 | p19 | p20 |
    p21 | p22 | p23 | p24 | p25 | p26 | p27 | p28 | p29 | p30 |
    p31 | p32 | p33 | p34 | p35 | p36 | p37 | p38 | p39 | p40 |
    p41 | p42 | p43 | p44 | p45 | p46 | p47 | p48 | p49 | p50 |
    p51 | p52 | p53 | p54 | p55 | p56 | p57 | p58 | p59 | p60 |
    p61 | p62 | p63 | p64 | p65 | p66 | p67 | p68 | p69 | p70 |
    p71 | p72 | p73 | p74 | p75 | p76 | p77 | p78 | p79 | p80 |
    p81 | p82 | p83 | p85 | p86 | p87 | p88 | p89 | p90 | p91 |
    p92 | p93 | p94 | p95 | p96 | p97 | p98 | p99 | p100 |
    p101 | p102 | p103 | p104 | p105 | p106 | p107 | p110 |
    p111 | p112 | p113 | p115 | p116 | p117 | p118 | p119 | p120 |
    p121 | p124 | p125 | p126 | p128 | p129 | p130 |
    p131 | p132 | p133 | p134 | p135 | p136 | p137 | p138 | p139 | p140 |
    p142 | p143 |
    r1 | r2 | r3 | r4 | r5 | r6 | r7 | r8 | r9 | r10 |
    r12 | r13 | r14 | r16 | r18 | r19 | r20 |
    t1 | t2 | t3 | t4 | t5 | t6 | t7 | t8 | t9 | t10 |
    t11 | t12 | t13 | t14 | t15 | t16 | t17 | t18 | t19 |
    u1 | u2 | u3 | u4 | u5 | u6 | u7 | u8 | u9 | u10 |
    u11 | u12 | u13 | u14 | u16 | u18 | u19 | u20 |
    v1 | v2 | v3 | v4 | v5 | v6 | v7 | v8 | v9 | v10 |
    v11 | v12 | v13 | v14 | v15 | v16 | v17 | v18 | v19 | v20 |
    w2 | w3 | w4 | w5 | w6 | w7 | w8 | w9 | w10 |
    w11 | w12 | w13 | w14 | w15 | w16 | w17 | w18 | w19 |
    y2 | y3 | y4 | y6 | y7 | y8 |
    y11 | y12 | y13 | y15 | y16 | y17 | y18 | y19 | y20.

