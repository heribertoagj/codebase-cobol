      ******************************************************************
      * SISTEMA : DCOM - I#DCOMMO                                      *
      * TAMANHO : 0512 BYTES                                           *
      * ARQUIVO : OPERACAO (EMISSAO AVISOS E EXTRATOS)                 *
      ******************************************************************
       01  MO-REGISTRO.
  1 10     03  MO-DMOVTC-DESC-COML          PIC  X(10).
 11  2     03  MO-COD-LANCAMENTO            PIC S9(03)V      COMP-3.
 13  4     03  MO-CINDCD-MOTVO-EVNTO-ORIGE  PIC  X(04).
 17  3     03  MO-DANO-OPER-DESC            PIC S9(04)V      COMP-3.
 20  5     03  MO-NSEQ-OPER-DESC            PIC S9(09)V      COMP-3.
 25  2     03  MO-CSIT-OPER-DESC            PIC S9(03)V      COMP-3.
 27  2     03  MO-CPRODT                    PIC S9(03)V      COMP-3.
 29  2     03  MO-CSPROD-DESC               PIC S9(03)V      COMP-3.
 31  2     03  MO-CTPO-DESC                 PIC S9(03)V      COMP-3.
 33  3     03  MO-CJUNC-DEPDC-CDENT         PIC S9(05)V      COMP-3.
 36  3     03  MO-CGRP-CTBIL-CDENT          PIC S9(05)V      COMP-3.
 39  2     03  MO-CSGRP-CTBIL-CDENT         PIC S9(03)V      COMP-3.
 41  7     03  MO-CCCORR-CDENT              PIC S9(13)V      COMP-3.
 48  5     03  MO-CCNPJ-CPF-CDENT           PIC S9(09)V      COMP-3.
 53  3     03  MO-CFLIAL-CNPJ-CDENT         PIC S9(05)V      COMP-3.
 56  2     03  MO-CCTRL-CNPJ-CDENT          PIC S9(02)V      COMP-3.
 58 60     03  MO-ICDENT                    PIC  X(60).
118  3     03  MO-CAG-BCRIA-CONTR-CONVE     PIC S9(05)V      COMP-3.
121  7     03  MO-CCCORR-CONTR-CONVE        PIC S9(13)V      COMP-3.
128  5     03  MO-CCNPJ-CPF-CONTR-CONVE     PIC S9(09)V      COMP-3.
133  3     03  MO-CFLIAL-CNPJ-CONTR-CONVE   PIC S9(05)V      COMP-3.
136  2     03  MO-CCTRL-CNPJ-CONTR-CONVE    PIC S9(02)V      COMP-3.
138 10     03  MO-DINIC-OPER-DESC           PIC  X(10).
148  2     03  MO-CMEIO-ENTRD-DESC          PIC S9(03)V      COMP-3.
150  9     03  MO-VOPER-DESC-COML           PIC S9(15)V9(02) COMP-3.
159  9     03  MO-VJURO-OPER-DESC           PIC S9(15)V9(02) COMP-3.
168  9     03  MO-VTAC-OPER-DESC            PIC S9(15)V9(02) COMP-3.
177  9     03  MO-VTARIF-TITLO-OPER-DESC    PIC S9(15)V9(02) COMP-3.
186  9     03  MO-VIOF-OPER-DESC            PIC S9(15)V9(02) COMP-3.
195  5     03  MO-CCNPJ-CPF-SACDO           PIC S9(09)V      COMP-3.
200  3     03  MO-CFLIAL-CNPJ-SACDO         PIC S9(05)V      COMP-3.
203  2     03  MO-CCTRL-CNPJ-SACDO          PIC S9(02)V      COMP-3.
205 60     03  MO-IPSSOA-SACDO              PIC  X(60).
265  1     03  MO-CTPO-CLIENTE              PIC S9(01)V      COMP-3.
266  3     03  MO-CGRP-CTBIL-CONVE          PIC S9(05)V      COMP-3.
269  2     03  MO-CSGRP-CTBIL-CONVE         PIC S9(03)V      COMP-3.
271 60     03  MO-ICCONVE                   PIC  X(60).
331  3     03  MO-CJUNC-DEPDC-SACDO         PIC S9(05)V      COMP-3.
334  3     03  MO-CGRP-CTBIL-SACDO          PIC S9(05)V      COMP-3.
337  2     03  MO-CSGRP-CTBIL-SACDO         PIC S9(03)V      COMP-3.
339  7     03  MO-CCCORR-SACDO              PIC S9(13)V      COMP-3.
346  5     03  MO-CCNPJ-CPF-DESTINO         PIC S9(09)V      COMP-3.
351  3     03  MO-CFLIAL-CNPJ-DESTINO       PIC S9(05)V      COMP-3.
354  2     03  MO-CCTRL-CNPJ-DESTINO        PIC S9(02)V      COMP-3.
356  2     03  MO-CINDCD-CONTR-CONVE        PIC  X(02).
      *------  DADOS COMPLEMENTARES  ------
358 10     03  MO-DINIC-PER-AV              PIC  X(10).
368 10     03  MO-DFNAL-PER-AV              PIC  X(10).
378 10     03  MO-NM-REDUZIDO               PIC  X(10).
388  2     03  MO-CSITUA                    PIC S9(03)V      COMP-3.
390  5     03  MO-CCART-VENCD               PIC  X(05).
395  5     03  MO-CCART-NORML               PIC  X(05).
400  3     03  MO-CJUNC-DEPDC-DEST          PIC S9(05)V      COMP-3.
403  7     03  MO-CCCORR-DEST               PIC S9(13)V      COMP-3.
410  3     03  MO-CGRP-CTBIL-DEST           PIC S9(05)V      COMP-3.
413  2     03  MO-CSGRP-CTBIL-DEST          PIC S9(03)V      COMP-3.
415  5     03  MO-CCONTR-CONVE-DESC         PIC S9(09)V      COMP-3.
420  2     03  MO-CVRSAO-CONTR-CONVE        PIC S9(03)V      COMP-3.
422  3     03  MO-CPOSTO-SERVC-CDENT        PIC S9(05)V      COMP-3.
425 10     03  MO-DVCTO-FNAL-OPER           PIC  X(10).
435  3     03  MO-TOPER-DESC                PIC S9(05)V      COMP-3.
438  3     03  MO-TMED-OPER                 PIC S9(05)V      COMP-3.
441  5     03  MO-QTITLO-CHEQ-OPER          PIC S9(09)V      COMP-3.
446  5     03  MO-PTX-JURO-ANO              PIC S9(03)V9(06) COMP-3.
451  5     03  MO-PTX-JURO-MES              PIC S9(03)V9(06) COMP-3.
456  3     03  MO-QPCELA-OPER               PIC S9(05)V      COMP-3.
459  5     03  MO-QTDE-CONTRATO             PIC S9(09)V      COMP-3.
464  2     03  MO-CTPO-GARNTIA-PRINC        PIC S9(03)V      COMP-3.
466  2     03  MO-CTPO-GARNTIA-COMPL        PIC S9(03)V      COMP-3.
468  2     03  MO-CTPO-PAB                  PIC  X(02).
470  2     03  MO-SEGMENTO-CLIE             PIC S9(03)V      COMP-3.
472 41     03  FILLER                       PIC  X(41).
