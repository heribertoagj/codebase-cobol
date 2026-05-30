      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMTJ       *
      * ARQUIVO = PARCELA OPERACAO ENTRADA  SEQUENCIAL                 *
      * LRECL   = 450 BYTES                 16-04-2007                 *
      *----------------------------------------------------------------*
BSI001*                    ALTERACAO - BSI TECNOLOGIA                 *
BSI001*---------------------------------------------------------------*
BSI001*  ANALISTA BSI:   FERNANDO LUIZ DE SANTI  -  BSI TECNOLOGIA    *
BSI001*  DATA........:   28/07/2017                                   *
BSI001*---------------------------------------------------------------*
BSI001*  OBJETIVO....:   INCLUIR CAMPOS DE ENCARGOS MORATORIOS.       *
BSI001*****************************************************************
       01  TJ-REGISTRO.
 01 03     05 TJ-DANO-OPER-DESC           PIC S9(004)          COMP-3.
 04 05     05 TJ-NSEQ-OPER-DESC           PIC S9(009)          COMP-3.
 09 03     05 TJ-NPCELA-DESC              PIC S9(005)          COMP-3.
 12 05     05 TJ-DEVNTO                   PIC  9(008)          COMP-3.
 17 02     05 TJ-CSIT-PCELA-DESC          PIC S9(003)          COMP-3.
 19 10     05 TJ-DVCTO-PCELA-DESC         PIC  X(010).
 29 05     05 TJ-QTITLO-CHEQ-PCELA        PIC S9(009)          COMP-3.
 34 09     05 TJ-VPRINC-PCELA-DESC        PIC S9(015)V9(02)    COMP-3.
 43 09     05 TJ-VIOF-PCELA-DESC          PIC S9(015)V9(02)    COMP-3.
 52 05     05 TJ-PTX-JURO-MES             PIC S9(003)V9(06)    COMP-3.
 57 05     05 TJ-PTX-JURO-ANO             PIC S9(003)V9(06)    COMP-3.
 62 09     05 TJ-VJURO-PCELA-DESC         PIC S9(015)V9(02)    COMP-3.
 71 09     05 TJ-VJURO-DIA-PCELA          PIC S9(015)V9(02)    COMP-3.
 80 09     05 TJ-VJURO-APROP-ACUM         PIC S9(015)V9(02)    COMP-3.
 89 09     05 TJ-VPSENT-PCELA-DESC        PIC S9(015)V9(02)    COMP-3.
 98 09     05 TJ-VSDO-DVDOR-PRINC         PIC S9(015)V9(02)    COMP-3.
107 09     05 TJ-VIOF-PCELA-VENCD         PIC S9(015)V9(02)    COMP-3.
116 09     05 TJ-VJURO-MORA-LEGAL         PIC S9(015)V9(02)    COMP-3.
125 09     05 TJ-VJURO-MORA-GERC          PIC S9(015)V9(02)    COMP-3.
134 10     05 TJ-DTRNSF-CRATS-PCELA       PIC  X(010).
144 09     05 TJ-VJURO-TRNSF-CRATS        PIC S9(015)V9(02)    COMP-3.
153 05     05 TJ-PTX-CUSTO-MES            PIC S9(003)V9(06)    COMP-3.
158 05     05 TJ-PTX-CUSTO-ANO            PIC S9(003)V9(06)    COMP-3.
163 09     05 TJ-VCUSTO-PCELA-DESC        PIC S9(015)V9(02)    COMP-3.
172 09     05 TJ-VCUSTO-PCELA-VENCD       PIC S9(015)V9(02)    COMP-3.
181 09     05 TJ-VCUSTO-TRNSF-CRATS       PIC S9(015)V9(02)    COMP-3.
190 05     05 TJ-CREFT-BACEN              PIC S9(009)          COMP-3.
195 04     05 TJ-CCONTR-CORP-PCELA        PIC S9(007)          COMP-3.
199 02     05 TJ-CBCO-DEPOS               PIC S9(003)          COMP-3.
201 03     05 TJ-CAG-BCRIA-DEPOS          PIC S9(005)          COMP-3.
204 10     05 TJ-DTRNSF-LP-PCELA          PIC  X(010).
214 02     05 TJ-CBCO-PRODT-COBR          PIC S9(003)          COMP-3.
216 02     05 TJ-CIDTFD-PRODT-COBR        PIC S9(002)          COMP-3.
218 10     05 TJ-CNEGOC-COBR              PIC S9(018)          COMP-3.
228 10     05 TJ-CTITLO-COBR-BCO          PIC S9(018)          COMP-3.
238 02     05 TJ-CNRO-SEQ-TITLO           PIC S9(003)          COMP-3.
240 03     05 TJ-CCONDC-PGTO-FORNC        PIC S9(005)          COMP-3.
243 16     05 TJ-CIDTFD-SEQ-PGTO          PIC  X(016).
259 01     05 TJ-CORIGE-PGTO-FORNC        PIC S9(001)          COMP-3.
260 05     05 TJ-CCNPJ-CPF-PGTO           PIC S9(009)          COMP-3.
265 03     05 TJ-CFLIAL-CNPJ-PGTO         PIC S9(005)          COMP-3.
268 02     05 TJ-CCTRL-CNPJ-PGTO          PIC S9(002)          COMP-3.
274 01     05 TJ-CFUNC-BDSCO              PIC S9(009)          COMP-3.
275 09     05 TJ-VALOR-TARIFA             PIC S9(015)V9(02)    COMP-3.
284 09     05 TJ-VCUSTO-DIA               PIC S9(015)V9(02)    COMP-3.
293 15     05 TJ-SEU-NUMERO               PIC X(015).
308 01     05 TJ-CINDCD-CALC-JURO         PIC 9(001).
     ****  TIPO: 1 - CALCULO LINEAR                              ***
     ****        2 - CALCULO EXPONENCIAL                         ***
309 09     05 TJ-VALOR-TAC                PIC S9(015)V9(02)    COMP-3.
318 02     05 TJ-CTPO-DESC-COML           PIC S9(003)          COMP-3.
320 10     05 TJ-DINIC-OPER-DESC          PIC X(010).
330 01     05 TJ-CALCULAR-DIF             PIC X(001).
331 03     05 TJ-CSIT-PCELA-ANT           PIC 9(003).
334 09     05 TJ-VCUST-APROP-ACUM         PIC S9(015)V9(02)    COMP-3.
343 05     05 TJ-DULT-PRROG-PCELA         PIC 9(008)           COMP-3.
348 09     05 TJ-VSDO-DVDOR-PRINC-BX      PIC S9(015)V9(02)    COMP-3.
BSI001*----------------------------------------------------------------*
BSI001*   CAMPOS PARA ENCARGOS MORATORIOS                              *
BSI001*----------------------------------------------------------------*
357 09     05 TJ-VJURO-REMUN-VENCD        PIC S9(015)V9(02) COMP-3.
366 09     05 TJ-VMORA-VENCD-DESC         PIC S9(015)V9(02) COMP-3.
375 09     05 TJ-VMULTA-VENCD-DESC        PIC S9(015)V9(02) COMP-3.
384 67     05 FILLER                      PIC X(067).
