      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMTH       *
      * ARQUIVO = MOTIVO EVENTO ORIGEM      SEQUENCIAL                 *
      * LRECL   = 800 BYTES                 08-05-2008                 *
      *----------------------------------------------------------------*
       01 TH-REGISTRO.
 01 30    05 TH-CHAVE-MVTO-C3.
 01 02       10 TH-CELMTO-DESC-COML          PIC S9(003)   USAGE COMP-3.
 03 02       10 TH-CEVNTO-DESC-COML          PIC S9(003)   USAGE COMP-3.
 05 26       10 TH-DHORA-ATULZ               PIC  X(026).
 31 03    05 TH-DANO-OPER-DESC               PIC S9(004)   USAGE COMP-3.
 34 05    05 TH-NSEQ-OPER-DESC               PIC S9(009)   USAGE COMP-3.
 39 03    05 TH-NPCELA-OPER-DESC             PIC S9(005)   USAGE COMP-3.
 42 05    05 TH-CCONTR-CONVE                 PIC S9(009)   USAGE COMP-3.
 47 02    05 TH-CVRSAO-CONTR-CONVE           PIC S9(003)   USAGE COMP-3.
 49 04    05 TH-CINDCD-MOTVO-EVNTO-ORIGE     PIC  X(004).
 53 03    05 TH-CTPO-ATULZ                   PIC  X(003).
 56 04    05 TH-CINDCD-MOTVO-EVNTO-ORIGE2    PIC  X(004).
 60 03    05 TH-CTPO-ATULZ2                  PIC  X(003).
 63 03    05 TH-NSEQ-REGISTRO                PIC S9(004)   USAGE COMP-3.
 66 35    05 TH-VAGO                         PIC  X(035).
      *----------------------------------------------------------------*
      *-------AREA DO DE/PARA (IDENTICO AO I#DCOMR6)---700 BYTES-------*
      *----------------------------------------------------------------*
          05  DCOMTH-REG-R6.
           15 TH-ADI-AGENCIA-CONTA.
              20  TH-CAG-BCRIA-DE             PIC  S9(05)      COMP-3.
              20  TH-CCTA-BCRIA-CLI-DE        PIC  S9(13)      COMP-3.
              20  TH-FILLER                   PIC   X(240).
              20  TH-CAG-BCRIA-PARA           PIC  S9(05)      COMP-3.
              20  TH-CCTA-BCRIA-CLI-PARA      PIC  S9(13)      COMP-3.
              20  FILLER                      PIC   X(240).

           15 TH-ADI-NRO-CONE      REDEFINES  TH-ADI-AGENCIA-CONTA.
              20  TH-CCART-DE                 PIC   X(05).
              20  TH-DANO-BASE-DE             PIC  S9(04)      COMP-3.
              20  TH-CDEPDC-DE                PIC  S9(05)      COMP-3.
              20  TH-CSEQ-STUDO-DE            PIC  S9(09)      COMP-3.
              20  TH-FILLER                   PIC   X(234).
              20  TH-CCART-PARA               PIC   X(05).
              20  TH-DANO-BASE-PARA           PIC  S9(04)      COMP-3.
              20  TH-CDEPDC-PARA              PIC  S9(05)      COMP-3.
              20  TH-CSEQ-STUDO-PARA          PIC  S9(09)      COMP-3.
              20  FILLER                      PIC   X(234).

           15 TH-ADI-TPO-GARANTIA  REDEFINES  TH-ADI-AGENCIA-CONTA.
              20  TH-CTPO-GARNT-DE-1          PIC  S9(03)      COMP-3.
              20  TH-CTPO-GARNT-DE-2          PIC  S9(03)      COMP-3.
              20  TH-FILLER                   PIC   X(246).
              20  TH-CTPO-GARNT-PARA-1        PIC  S9(03)      COMP-3.
              20  TH-CTPO-GARNT-PARA-2        PIC  S9(03)      COMP-3.
              20  TH-FILLER                   PIC   X(246).

           15 TH-ADI-TESTEMUNHA    REDEFINES  TH-ADI-AGENCIA-CONTA.
              20  TH-IPSSOA-DE                PIC   X(60).
              20  TH-CCNPJ-CPF-DE             PIC  S9(09)      COMP-3.
              20  TH-CFLIAL-CNPJ-DE           PIC  S9(05).
              20  TH-CCTRL-CNPJ-CPF-DE        PIC  S9(02)      COMP-3.
              20  TH-FILLER                   PIC   X(180).
              20  TH-IPSSOA-PARA              PIC   X(60).
              20  TH-CCNPJ-CPF-PARA           PIC  S9(09)      COMP-3.
              20  TH-CFLIAL-CNPJ-PARA         PIC  S9(05).
              20  TH-CCTRL-CNPJ-CPF-PARA      PIC  S9(02)      COMP-3.
              20  TH-FILLER                   PIC   X(176).

           15 TH-ADI-AVALISTA     REDEFINES   TH-ADI-AGENCIA-CONTA.
              20  TH-CCNPJ-CPF-AVAL-DE        PIC  S9(09)      COMP-3.
              20  TH-CFLIAL-CNPJ-AVAL-DE      PIC  S9(05)      COMP-3.
              20  TH-CCTRL-AVAL-DE            PIC  S9(02)      COMP-3.
              20  TH-IPSSOA-AVAL-DE           PIC   X(40).
              20  TH-ELOGDR-DE                PIC   X(40).
              20  TH-IBAIRO-DE                PIC   X(40).
              20  TH-IMUN-DE                  PIC   X(30).
              20  TH-CCEP-DE                  PIC  S9(05)      COMP-3.
              20  TH-CCEP-COMPL-DE            PIC  S9(03)      COMP-3.
              20  TH-CSGL-UF-DE               PIC   X(02).
              20  TH-CDOCTO-ID-AVAL-DE        PIC   X(15).
              20  TH-CINDCD-CONJG-AUTRZ-DE    PIC   X(01).
              20  TH-IPSSOA-CONJG-DE          PIC   X(40).
              20  TH-CPF-CONJG-DE             PIC  S9(09)      COMP-3.
              20  TH-CCTRL-CPF-CONJG-DE       PIC  S9(02)      COMP-3.
              20  TH-CDOCTO-ID-CONJG-DE       PIC   X(15).
              20  TH-NRELAC-PSSOA-DE          PIC  S9(09)      COMP-3.
              20  TH-CCNPJ-CPF-AVAL-PARA      PIC  S9(09)      COMP-3.
              20  TH-CFLIAL-AVAL-PARA         PIC  S9(05)      COMP-3.
              20  TH-CCTRL-AVAL-PARA          PIC  S9(02)      COMP-3.
              20  TH-IPSSOA-AVAL-PARA         PIC   X(40).
              20  TH-ELOGDR-PARA              PIC   X(40).
              20  TH-IBAIRO-PARA              PIC   X(40).
              20  TH-IMUN-PARA                PIC   X(30).
              20  TH-CCEP-PARA                PIC  S9(05)      COMP-3.
              20  TH-CCEP-COMPL-PARA          PIC  S9(03)      COMP-3.
              20  TH-CSGL-UF-PARA             PIC   X(02).
              20  TH-CDOCTO-ID-AVAL-PARA      PIC   X(15).
              20  TH-CINDCD-CONJG-AUTRZ-PARA  PIC   X(01).
              20  TH-IPSSOA-CONJG-PARA        PIC   X(40).
              20  TH-CPF-CONJG-PARA           PIC  S9(09)      COMP-3.
              20  TH-CCTRL-CPF-CONJG-PARA     PIC  S9(02)      COMP-3.
              20  TH-CDOCTO-ID-CONJG-PARA     PIC   X(15).
              20  TH-NRELAC-PSSOA-PARA        PIC  S9(09)      COMP-3.

           15 TH-ADI-TAXA-OPERACAO REDEFINES  TH-ADI-AGENCIA-CONTA.
              20  TH-PERC-TAXA-MES-DE         PIC  S9(03)V9(5) COMP-3.
              20  TH-PERC-TAXA-ANO-DE         PIC  S9(03)V9(5) COMP-3.
              20  TH-VALOR-JUROS-DE           PIC  S9(15)V9(2) COMP-3.
              20  TH-CAUTRZ-TX-TESOU-DE       PIC  S9(09)      COMP-3.
              20  TH-IAUTRZ-TX-TESOU-DE       PIC   X(40).
              20  TH-DESCR-TIPO-TAXA-DE       PIC   X(20).
              20  TH-VALOR-IOF-DE             PIC  S9(07)V9(2) COMP-3.
              20  TH-FILLER                   PIC   X(157).
              20  TH-PERC-TAXA-MES-PARA       PIC  S9(03)V9(5) COMP-3.
              20  TH-PERC-TAXA-ANO-PARA       PIC  S9(03)V9(5) COMP-3.
              20  TH-VALOR-JUROS-PARA         PIC  S9(15)V9(2) COMP-3.
              20  TH-CAUTRZ-TX-TESOU-PARA     PIC  S9(09)      COMP-3.
              20  TH-IAUTRZ-TX-TESOU-PARA     PIC   X(40).
              20  TH-DESCR-TIPO-TAXA-PARA     PIC   X(20).
              20  TH-VALOR-IOF-PARA           PIC  S9(07)V9(2) COMP-3.
              20  TH-FILLER                   PIC   X(157).
              20  TH-PMIN-TX-JURO-REGRA       PIC  S9(03)V9(3) COMP-3.
              20  TH-PMAX-TX-JURO-REGRA       PIC  S9(03)V9(3) COMP-3.

           15 TH-ADI-TITULOS       REDEFINES  TH-ADI-AGENCIA-CONTA.
              20  TH-DEMIS-TITLO-DE           PIC   X(10).
              20  TH-DVCTO-TITLO-DESC-DE      PIC   X(10).
              20  TH-VTITLO-DESC-COML-DE      PIC  S9(15)V9(2) COMP-3.
              20  TH-CSEU-NRO-TITLO-DE        PIC   X(15).
              20  TH-CCNPJ-CPF-DE             PIC  S9(09)      COMP-3.
              20  TH-CFLIAL-CNPJ-DE           PIC  S9(05)      COMP-3.
              20  TH-CCTRL-CNPJ-DE            PIC  S9(02)      COMP-3.
              20  TH-IPSSOA-SACDO-DE          PIC   X(40).
              20  TH-CCEP-DE                  PIC  S9(05)      COMP-3.
              20  TH-CCEP-COMPL-DE            PIC  S9(03)      COMP-3.
              20  TH-ELOGDR-DE                PIC   X(40).
              20  TH-IBAIRO-DE                PIC   X(30).
              20  TH-IMUN-DE                  PIC   X(30).
              20  TH-CSGL-UF-DE               PIC   X(02).
              20  TH-VJURO-PCELA-DESC-DE      PIC  S9(15)V9(2) COMP-3.
              20  TH-VJURO-DIA-PCELA-DE       PIC  S9(15)V9(2) COMP-3.
              20  TH-VIOF-PCELA-DESC-DE       PIC  S9(15)V9(2) COMP-3.
              20  TH-FILLER                   PIC   X(22).
              20  TH-DEMIS-TITLO-PARA         PIC   X(10).
              20  TH-DVCTO-TITLO-DESC-PARA    PIC   X(10).
              20  TH-VTITLO-DESC-COML-PARA    PIC  S9(15)V9(2) COMP-3.
              20  TH-CSEU-NRO-TITLO-PARA      PIC   X(15).
              20  TH-CCNPJ-CPF-PARA           PIC  S9(09)      COMP-3.
              20  TH-CFLIAL-CNPJ-PARA         PIC  S9(05)      COMP-3.
              20  TH-CCTRL-CNPJ-PARA          PIC  S9(02)      COMP-3.
              20  TH-IPSSOA-SACDO-PARA        PIC   X(40).
              20  TH-CCEP-PARA                PIC  S9(05)      COMP-3.
              20  TH-CCEP-COMPL-PARA          PIC  S9(03)      COMP-3.
              20  TH-ELOGDR-PARA              PIC   X(40).
              20  TH-IBAIRO-PARA              PIC   X(30).
              20  TH-IMUN-PARA                PIC   X(30).
              20  TH-CSGL-UF-PARA             PIC   X(02).
              20  TH-VJURO-PCELA-DESC-PARA    PIC  S9(15)V9(2) COMP-3.
              20  TH-VJURO-DIA-PCELA-PARA     PIC  S9(15)V9(2) COMP-3.
              20  TH-VIOF-PCELA-DESC-PARA     PIC  S9(15)V9(2) COMP-3.
              20  TH-FILLER                   PIC   X(22).

           15 TH-ADI-INSTRUCAO    REDEFINES   TH-ADI-AGENCIA-CONTA.
              20  TH-DINSTR-DESC-PCELA1-DE    PIC   X(10).
              20  TH-VINSTR-DESC-PCELA1-DE    PIC  S9(15)V9(2) COMP-3.
              20  TH-PINSTR-DESC-PCELA1-DE    PIC  S9(03)V9(3) COMP-3.
              20  TH-DINSTR-DESC-PCELA2-DE    PIC   X(10).
              20  TH-VINSTR-DESC-PCELA2-DE    PIC  S9(15)V9(2) COMP-3.
              20  TH-PINSTR-DESC-PCELA2-DE    PIC  S9(03)V9(3) COMP-3.
              20  TH-FILLER                   PIC   X(227).
              20  TH-DINSTR-DESC-PCELA1-PARA  PIC   X(10).
              20  TH-VINSTR-DESC-PCELA1-PARA  PIC  S9(15)V9(2) COMP-3.
              20  TH-PINSTR-DESC-PCELA1-PARA  PIC  S9(03)V9(3) COMP-3.
              20  TH-DINSTR-DESC-PCELA2-PARA  PIC   X(10).
              20  TH-VINSTR-DESC-PCELA2-PARA  PIC  S9(15)V9(2) COMP-3.
              20  TH-PINSTR-DESC-PCELA2-PARA  PIC  S9(03)V9(3) COMP-3.
              20  TH-FILLER                   PIC   X(181).

           15 TH-ADI-SAC-AVAL      REDEFINES  TH-ADI-AGENCIA-CONTA.
              20  TH-CINDCD-PSSOA-DESC-DE     PIC   X(02).
              20  TH-CELMTO-DESC-COML-DE      PIC  S9(03)      COMP-3.
              20  TH-CCHAVE-ELMTO-DESC-DE     PIC   X(45).
              20  TH-CCNPJ-SAC-AVAL-DE        PIC  S9(09)      COMP-3.
              20  TH-CFLIAL-SAC-AVAL-DE       PIC  S9(05)      COMP-3.
              20  TH-CCTRL-SAC-AVAL-DE        PIC  S9(02)      COMP-3.
              20  TH-IPSSOA-SAC-AVAL-DE       PIC   X(40).
              20  TH-FILLER                   PIC   X(151).
              20  TH-CINDCD-PSSOA-DESC-PARA   PIC   X(02).
              20  TH-CELMTO-DESC-COML-PARA    PIC  S9(03)      COMP-3.
              20  TH-CCHAVE-ELMTO-DESC-PARA   PIC   X(45).
              20  TH-CCNPJ-SAC-AVAL-PARA      PIC  S9(09)      COMP-3.
              20  TH-CFLIAL-SAC-AVAL-PARA     PIC  S9(05)      COMP-3.
              20  TH-CCTRL-SAC-AVAL-PARA      PIC  S9(02)      COMP-3.
              20  TH-IPSSOA-SAC-AVAL-PARA     PIC   X(40).
              20  TH-FILLER                   PIC   X(151).

           15 TH-ADI-VALOR-OPER    REDEFINES  TH-ADI-AGENCIA-CONTA.
              20  TH-VOPER-DESC-COML-DE       PIC  S9(15)V9(2) COMP-3.
              20  TH-VLIQ-OPER-DESC-DE        PIC  S9(15)V9(2) COMP-3.
              20  TH-FILLER                   PIC   X(232).
              20  TH-VOPER-DESC-COML-PARA     PIC  S9(15)V9(2) COMP-3.
              20  TH-VLIQ-OPER-DESC-PARA      PIC  S9(15)V9(2) COMP-3.
              20  FILLER                      PIC   X(232).

           15 TH-PRORROGA-PARCELA  REDEFINES  TH-ADI-AGENCIA-CONTA.
              20  TH-DVCTO-PCELA-DESC-DE      PIC   X(10).
              20  TH-PTX-JURO-MES-DE          PIC  S9(03)V9(5) COMP-3.
              20  TH-VJURO-PRROG-PCELA-DE     PIC  S9(15)V9(2) COMP-3.
              20  TH-PIOF-PRROG-PCELA-DE      PIC  S9(03)V9(5) COMP-3.
              20  TH-VIOF-PRROG-PCELA-DE      PIC  S9(15)V9(2) COMP-3.
              20  TH-VTAXA-PRROG-PCELA-DE     PIC  S9(15)V9(2) COMP-3.
              20  TH-VVLOR-PCELA-DE           PIC  S9(15)V9(2) COMP-3.
              20  TH-TFREQ-TAXA-PCELA-DE      PIC   X(04).
              20  TH-VJURO-DIA-PRROG-DE       PIC  S9(15)V9(2) COMP-3.
              20  TH-FILLER                   PIC   X(181).
              20  TH-DVCTO-PCELA-DESC-PARA    PIC   X(10).
              20  TH-PTX-JURO-MES-PARA        PIC  S9(03)V9(5) COMP-3.
              20  TH-VJURO-PRROG-PCELA-PARA   PIC  S9(15)V9(2) COMP-3.
              20  TH-PIOF-PRROG-PCELA-PARA    PIC  S9(03)V9(5) COMP-3.
              20  TH-VIOF-PRROG-PCELA-PARA    PIC  S9(15)V9(2) COMP-3.
              20  TH-VTAXA-PRROG-PCELA-PARA   PIC  S9(15)V9(2) COMP-3.
              20  TH-TFREQ-TAXA-PCELA-PARA    PIC   X(04).
              20  TH-VJURO-DIA-PRROG-PARA     PIC  S9(15)V9(2) COMP-3.
              20  TH-PMIN-TAXA-JURO-PARA      PIC  S9(03)V9(3) COMP-3.
              20  TH-PMAX-TAXA-JURO-PARA      PIC  S9(03)V9(3) COMP-3.
              20  TH-CTPO-TX-JURO             PIC   9(03).
              20  TH-ITPO-TX-JURO             PIC   X(15).
              20  TH-FILLER                   PIC   X(164).

           15 TH-DADOS-GERAIS.
              20  TH-CPRODT                   PIC  S9(03)      COMP-3.
              20  TH-CSPROD-DESC-COML         PIC  S9(03)      COMP-3.
              20  TH-CBCO-OPER-OPER           PIC  S9(03)      COMP-3.
              20  TH-CAG-OPER-OPER            PIC  S9(05)      COMP-3.
              20  TH-CTPO-AUTRZ-ESPCL         PIC  S9(03)      COMP-3.
              20  TH-CAUTRZ-ESPCL             PIC  S9(09)      COMP-3.
              20  TH-IAUTRZ-ESPCL             PIC   X(40).
              20  TH-CIND-COBR-TAC            PIC   X(01).
              20  TH-CINDCD-FREQ-TX           PIC   X(02).
              20  TH-FILLER                   PIC   X(41).

           15 TH-SENSIBILIZA-CONE.
              20  TH-TITULO-RECUSADO-ANLSE    PIC   X(01).
              20  TH-CELMTO-DESC-COML-CONE    PIC   9(03) COMP-3.
              20  TH-CCONTR-CONVE-DESC-CONE   PIC   9(09) COMP-3.
              20  TH-CVRSAO-CONTR-CONVE-CONE  PIC   9(03) COMP-3.
              20  TH-DANO-BASE                PIC   9(04) COMP-3.
              20  TH-CDEPDC                   PIC   9(05) COMP-3.
              20  TH-CSEQ-STUDO               PIC   9(09) COMP-3.
              20  TH-CCART-NORML-DESC         PIC   X(05).
              20  TH-CCNPJ-CPF                PIC   9(09) COMP-3.
              20  TH-CFLIAL-CNPJ              PIC   9(05) COMP-3.
              20  TH-CCTRL-CNPJ-CPF           PIC   9(02) COMP-3.
              20  TH-FILLER                   PIC   X(64).
