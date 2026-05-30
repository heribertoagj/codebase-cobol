      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMOI       *
      * ARQUIVO = PARCELA OPERACAO DESCONTO SEQUENCIAL                 *
BSI001* LRECL   = 700 BYTES                 19-06-2007                 *
      *----------------------------------------------------------------*
BSI001*                    ALTERACAO - BSI TECNOLOGIA                 *
BSI001*---------------------------------------------------------------*
BSI001*  ANALISTA BSI:   FERNANDO LUIZ DE SANTI  -  BSI TECNOLOGIA    *
BSI001*  DATA........:   28/07/2017                                   *
BSI001*---------------------------------------------------------------*
BSI001*  OBJETIVO....:   INCLUIR CAMPOS DE ENCARGOS MORATORIOS.       *
BSI001*****************************************************************
       01 OI-REGISTRO.
 01 24    05 OI-CCHAVE-REG.
 01 04       10 OI-CCTRO-CUSTO           PIC  X(004).
 05 04       10 OI-CEVNTO-ORIGE          PIC  9(004).
 09 08       10 OI-DMOVTO                PIC  9(008).
 17 01       10 OI-CATULZ                PIC  X(001).
 18 01       10 OI-CINDCD-RPROC          PIC  X(001).
 19 06       10 OI-DCOMPT                PIC  9(006).
 25 16    05 OI-CHV-ENTID-PCELA-OPER-DESC.
 25 11       10 OI-CCHAVE-PCELA-OPER-DESC.
 25 03          15 OI-DANO-OPER-DESC     PIC S9(004)             COMP-3.
 28 05          15 OI-NSEQ-OPER-DESC     PIC S9(009)             COMP-3.
 33 03          15 OI-NPCELA-DESC        PIC S9(005)             COMP-3.
 36 05       10 OI-DEVNTO                PIC  9(008)             COMP-3.
 41 02    05 OI-CSIT-PCELA-DESC          PIC S9(003)             COMP-3.
 43 10    05 OI-DVCTO-PCELA-DESC         PIC  X(010).
 53 02    05 OI-TPCELA-DESC              PIC S9(003)             COMP-3.
 55 05    05 OI-QTITLO-CHEQ-PCELA        PIC S9(009)             COMP-3.
 60 09    05 OI-VTARIF-PCELA             PIC S9(015)V9(02)       COMP-3.
 69 09    05 OI-VPRINC-PCELA-DESC        PIC S9(015)V9(02)       COMP-3.
 78 09    05 OI-VIOF-PCELA-DESC          PIC S9(015)V9(02)       COMP-3.
 87 05    05 OI-PTX-JURO-MES             PIC S9(003)V9(06)       COMP-3.
 92 05    05 OI-PTX-JURO-ANO             PIC S9(003)V9(06)       COMP-3.
 97 09    05 OI-VJURO-PCELA-DESC         PIC S9(015)V9(02)       COMP-3.
106 09    05 OI-VJURO-DIA-PCELA          PIC S9(015)V9(02)       COMP-3.
115 09    05 OI-VJURO-DATA-EVNTO         PIC S9(015)V9(02)       COMP-3.
124 09    05 OI-VJURO-APROP-ACUM         PIC S9(015)V9(02)       COMP-3.
133 09    05 OI-VPSENT-PCELA-DESC        PIC S9(015)V9(02)       COMP-3.
142 09    05 OI-VRAP-PSENT-PCELA         PIC S9(015)V9(02)       COMP-3.
151 09    05 OI-VSDO-DVDOR-PRINC         PIC S9(015)V9(02)       COMP-3.
160 09    05 OI-VIOF-PCELA-VENCD         PIC S9(015)V9(02)       COMP-3.
169 09    05 OI-VJURO-MORA-LEGAL         PIC S9(015)V9(02)       COMP-3.
178 09    05 OI-VJURO-MORA-GERC          PIC S9(015)V9(02)       COMP-3.
187 10    05 OI-DTRNSF-CRATS-PCELA       PIC  X(010).
197 09    05 OI-VJURO-TRNSF-CRATS        PIC S9(015)V9(02)       COMP-3.
206 09    05 OI-VJURO-PROVS-CRATS        PIC S9(015)V9(02)       COMP-3.
215 09    05 OI-VJURO-DIFCA-CRATS        PIC S9(015)V9(02)       COMP-3.
224 09    05 OI-VJURO-DIFCA-MORA         PIC S9(015)V9(02)       COMP-3.
233 09    05 OI-VJURO-VNCER-ACUM         PIC S9(015)V9(02)       COMP-3.
242 09    05 OI-VJURO-MORA-ACUM          PIC S9(015)V9(02)       COMP-3.
251 09    05 OI-VJURO-CRATS-ACUM         PIC S9(015)V9(02)       COMP-3.
260 05    05 OI-PTX-CUSTO-MES            PIC S9(003)V9(06)       COMP-3.
265 05    05 OI-PTX-CUSTO-ANO            PIC S9(003)V9(06)       COMP-3.
270 05    05 OI-PCUSTO-DIA-VENCD         PIC S9(003)V9(06)       COMP-3.
275 03    05 OI-CINDXD-TX-REFT           PIC S9(005)             COMP-3.
278 09    05 OI-VCUSTO-DIA-PCELA         PIC S9(015)V9(02)       COMP-3.
287 09    05 OI-VCUSTO-PCELA-DESC        PIC S9(015)V9(02)       COMP-3.
296 09    05 OI-VCUSTO-PCELA-VENCD       PIC S9(015)V9(02)       COMP-3.
305 09    05 OI-VCUSTO-TRNSF-CRATS       PIC S9(015)V9(02)       COMP-3.
314 09    05 OI-VCUSTO-PROVS-CRATS       PIC S9(015)V9(02)       COMP-3.
323 09    05 OI-VCUSTO-VNCER-ACUM        PIC S9(015)V9(02)       COMP-3.
332 09    05 OI-VCUSTO-MORA-ACUM         PIC S9(015)V9(02)       COMP-3.
341 09    05 OI-VCUSTO-CRATS-ACUM        PIC S9(015)V9(02)       COMP-3.
350 05    05 OI-CREFT-BACEN              PIC S9(009)             COMP-3.
355 04    05 OI-CCONTR-CORP-PCELA        PIC S9(007)             COMP-3.
359 05    05 OI-CCHAVE-ID-AG-BCRIA-DEPOS.
359 02       10 OI-CBCO-DEPOS            PIC S9(003)             COMP-3.
361 03       10 OI-CAG-BCRIA-DEPOS       PIC S9(005)             COMP-3.
364 10    05 OI-DTRNSF-LP-PCELA          PIC  X(010).
374 26    05 OI-CCHAVE-ID-TITLO-COBR.
374 02       10 OI-CBCO-PRODT-COBR       PIC S9(003)             COMP-3.
376 02       10 OI-CIDTFD-PRODT-COBR     PIC S9(002)             COMP-3.
378 09       10 OI-CNEGOC-COBR           PIC S9(017)             COMP-3.
387 09       10 OI-CTITLO-COBR-BCO       PIC S9(017)             COMP-3.
386 02       10 OI-CNRO-SEQ-TITLO        PIC S9(003)             COMP-3.
488 30    05 OI-CCHAVE-ID-AGNDA-PGTO-FORNC.
498 03       10 OI-CCONDC-PGTO-FORNC     PIC S9(005)             COMP-3.
401 16       10 OI-CIDTFD-SEQ-PGTO       PIC  X(016).
417 01       10 OI-CORIGE-PGTO-FORNC     PIC S9(001)             COMP-3.
418 10       10 OI-CCHAVE-ID-CLI-PAGFOR.
418 05          15 OI-CCNPJ-CPF-PGTO     PIC  S9(009)            COMP-3.
423 03          15 OI-CFLIAL-CNPJ-PGTO   PIC  S9(005)            COMP-3.
426 02          15 OI-CCTRL-CNPJ-PGTO    PIC  S9(002)            COMP-3.
428 78    05 OI-INFO-CAD-SACDO.
428 01       10 OI-CINDCD-SACDO-CRRTT    PIC  X(001).
439 06       10 OI-CCLUB-SACDO           PIC S9(010)             COMP-3.
435 01       10 OI-CTPO-PSSOA-SACDO      PIC  X(001).
436 10       10 OI-ID-SACDO.
436 05          15 OI-CCNPJ-CPF-SACDO   PIC  S9(009)             COMP-3.
441 03          15 OI-CFLIAL-CNPF-SACDO  PIC S9(005)             COMP-3.
444 02          15 OI-CCTRL-CNPJ-SACDO   PIC S9(002)             COMP-3.
446 60       10 OI-IPSSOA-SACDO          PIC  X(060).
506 06    05 OI-PTIR-JURO-PCELA          PIC S9(004)V9(06)       COMP-3.
512 06    05 OI-PTIR-TRIBT-PCELA         PIC S9(004)V9(06)       COMP-3.
518 06    05 OI-PTIR-TARIF-PCELA         PIC S9(004)V9(06)       COMP-3.
524 15    05 OI-SEU-NUMERO               PIC  X(015).
      *----------------------------------------------------------------*
      *-  CARREGA QDO FOR SACADO EMITENTE (CTPO-DESC-COML = 7 OU 8)   -*
      *----------------------------------------------------------------*
539 02    05 OI-BCO-SACDO                PIC  9(003)             COMP-3.
541 03    05 OI-AGE-SACDO                PIC  9(005)             COMP-3.
544 07    05 OI-CTA-SACDO                PIC  9(013)             COMP-3.
551 02    05 OI-CGRP-CTBIL-SACDO         PIC  9(003)             COMP-3.
553 02    05 OI-CSGRP-CTBIL-SACDO        PIC  9(003)             COMP-3.
555 09    05 OI-VALOR-TAC                PIC S9(015)V9(02)       COMP-3.
564 02    05 OI-CTPO-DESC-COML           PIC S9(003)             COMP-3.
566 10    05 OI-DINIC-OPER-DESC          PIC X(010).
576 09    05 OI-VPRINC-ENTRADA           PIC S9(15)V99           COMP-3.
585 09    05 OI-VPRINC-BAIXA             PIC S9(15)V99           COMP-3.
      *----------------------------------------------------------------*
      *   TPO-VALOR = 1 ==>  VALOR PRINCIPAL > 130.000,00              *
      *   TPO-VALOR = 2 ==>  VALOR PRINCIPAL < 130.000,01              *
      *----------------------------------------------------------------*
594 01    05 OI-TPO-VALOR                PIC  9(01)              COMP-3.
      *----------------------------------------------------------------*
      *   TPO-CALC  = L - LINEAR / E - EXPONENCIAL                     *
      *----------------------------------------------------------------*
595 01    05 OI-TPO-CALC                 PIC  X(01).
596 03    05 OI-CSIT-PCELA-ANT           PIC  9(03).
      *----------------------------------------------------------------*
      *---------------------OI-CALCULAR-DIF ---------------------------*
      *   Y = VOLTA SITUACAO E MANDA ULTIMA APROPRIACAO DA MESMA       *
      *       QUANDO DATA DO VENCIMENTO IGUAL A DATA DO MOVIMENTO      *
      *       (VPRINC - VPSENT-ATUAL)                                  *
      *   S = BAIXA APOS O VENCIMENTO (VPRINC - VPSENT-ATUAL)          *
      *   N = BAIXA ANTECIPADA OU NO DIA DO VENCIMENTO                 *
      *   A = PAGTO CHEQ VINCULADO - ENVIAR ULTIMA DIA DE APROP        *
      *----------------------------------------------------------------*
599 01    05 OI-CALCULAR-DIF             PIC  X(01).
      *   OI-TRANSFERIU-CA  = S - SIM (ESTA SENDO TRANSFERIDO P/ C.A  )*
600 01    05 OI-TRANSFERIU-CA            PIC  X(01).
BSI001*----------------------------------------------------------------*
BSI001*   CAMPOS PARA ENCARGOS MORATORIOS                              *
BSI001*----------------------------------------------------------------*
601 09    05 OI-VJURO-REMUN-VENCD        PIC S9(015)V9(02) COMP-3.
610 09    05 OI-VMORA-VENCD-DESC         PIC S9(015)V9(02) COMP-3.
619 09    05 OI-VMULTA-VENCD-DESC        PIC S9(015)V9(02) COMP-3.
BSI001*----------------------------------------------------------------*
BSI001*   AREA PARA FILLER DO ARQUIVO                                  *
BSI001*----------------------------------------------------------------*
628 73    05 OI-FILLER                   PIC  X(73).
