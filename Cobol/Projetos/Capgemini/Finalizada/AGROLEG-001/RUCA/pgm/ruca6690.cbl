      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.                     RUCA6690.
       AUTHOR.                         RUSSO.
      *================================================================*
      *                     B  R  A  D  E  S  C  O                     *
      *================================================================*
      *     PROGRAMA....: RUCA6690                                     *
      *     ANALISTA....: RODRIGO KONSTANTINOVAS                       *
      *     DATA........: 04/03/2024                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: CONSULTAR DESCRITIVO DE CREDITO DA OPERACAO  *
      *                   RESOLUCAO 5004                               *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     RUCAT690....: CONSULTA DE DESCRITIVO DE CREDITO DA OPERACAO*
      *     RUCAP690....: IMPRESSAO   DESCRITIVO DE CREDITO DA OPERACAO*
      *----------------------------------------------------------------*
      *     INC'S.......:                                              *
      *     I#BRAD7C - AREA DE ERRO DA BRAD7100                        *
      *----------------------------------------------------------------*
      *     MODULOS.....:                                              *
      *     BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA *
      *     BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO    *
      *     POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC     *
      *     RUCA6691 - MODULO DE IMPRESSAO DA TELA - MAPA RUCAP690     *
      *     RUEC8880 - CALCULA CET                                     *
      *----------------------------------------------------------------*
      *     NAVEGACAO...:                                              *
      *     CHAMADO POR.:                    CHAMA:                    *
      *             RUCA6630                                           *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       DATA                             DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '** RUCA6690 - AREA DE WORKING **'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** VARIAVEIS AUXILIARES     ***'.
      *----------------------------------------------------------------*

       01  FILLER.
           03  IND-1                   PIC  9(009) COMP-3  VALUE ZEROS.
           03  IND-3                   PIC  9(005) COMP-3  VALUE ZEROS.
           03  IND-HEX                 PIC  9(005) COMP-3  VALUE ZEROS.
           03  IND-HEX2                PIC  9(005) COMP-3  VALUE ZEROS.
           03  WRK-DESPREZAR           PIC  9(009) COMP-3  VALUE ZEROS.
           03  WRK-SEQ-PCELA           PIC  9(009) COMP-3  VALUE ZEROS.
           03  WRK-PAGINA              PIC  9(003) COMP-3  VALUE ZEROS.
           03  WRK-FIM-CSR             PIC  X(001)         VALUE SPACES.
           03  WRK-CTR-LP              PIC  X(001)         VALUE SPACES.

       01  WRK-MODULO                  PIC  X(08)          VALUE SPACES.

       01  WRK-CONTROLE-AUX            PIC  9(004) COMP    VALUE 128.
       01  FILLER                      REDEFINES   WRK-CONTROLE-AUX.
           05  FILLER                  PIC  X(001).
           05  WRK-BYTE-LIGADO         PIC  X(001).

       01  WRK-CONTR-LIQUIDADO         PIC  X(001)         VALUE SPACES.

       01  WRK-DATA-CORRENTE           PIC  X(10)          VALUE SPACES.

       01  WRK-DATA-CURR-AAAAMMDD.
           03  WRK-AAAA-CUR            PIC  9(004)         VALUE ZEROS.
           03  WRK-MM-CUR              PIC  9(002)         VALUE ZEROS.
           03  WRK-DD-CUR              PIC  9(002)         VALUE ZEROS.
       01  WRK-DATA-CURR-R             REDEFINES
                                       WRK-DATA-CURR-AAAAMMDD PIC 9(08).

       01  WRK-DATA-VCTO-AAAAMMDD.
           03  WRK-AAAA-VCT            PIC  9(004)         VALUE ZEROS.
           03  WRK-MM-VCT              PIC  9(002)         VALUE ZEROS.
           03  WRK-DD-VCT              PIC  9(002)         VALUE ZEROS.
       01  WRK-DATA-VCTO-R             REDEFINES
                                       WRK-DATA-VCTO-AAAAMMDD PIC 9(08).

       01  WRK-DATA-DB2.
           03  WRK-DD-DB2              PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  WRK-MM-DB2              PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  WRK-AAAA-DB2            PIC  9(004)         VALUE ZEROS.

       01  WRK-DATA-BARRA.
           03  WRK-DD                  PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  WRK-MM                  PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  WRK-AAAA                PIC  9(004)         VALUE ZEROS.

       01  WRK-MDOPER.
           03  WRK-IFNALD-RURAL        PIC  X(019)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE '-'.
           03  WRK-IATVDD-RURAL        PIC  X(020)         VALUE SPACES.

       01  WRK-COUNT                   PIC S9(009) COMP    VALUE ZEROS.
       01  WRK-ACU-TPCREM              PIC S9(009) COMP-3  VALUE ZEROS.

       01  WRK-NUM-09-CS               PIC +9(009)         VALUE ZEROS.
       01  FILLER REDEFINES            WRK-NUM-09-CS.
           05 FILLER                   PIC  X(001).
           05 WRK-NUM-09-SS            PIC  9(009).

       01  WRK-NRO-09                  PIC  9(009)         VALUE ZEROS.
       01  FILLER REDEFINES            WRK-NRO-09.
           05 FILLER                   PIC  X(006).
           05 WRK-NRO-03               PIC  9(003).

       01 WRK-ACU-SDVATU               PIC S9(013)V9(02) COMP-3
                                                            VALUE ZEROS.
MJ0001 01 WRK-VLJUR-RUMO               PIC S9(013)V9(02) COMP-3
MJ0001                                                      VALUE ZEROS.
MJ0001 01 WRK-VALOR-JUR-AUX            PIC S9(013)V9(02) COMP-3
MJ0001                                                      VALUE ZEROS.

       01  WRK-VALOR-S13-D2            PIC +9(13)V9(02)    VALUE ZEROS.
       01  FILLER REDEFINES WRK-VALOR-S13-D2.
           03  WRK-VALOR-SINAL         PIC X(001).
           03  WRK-VALOR-Z13-D2        PIC 9(013)V9(02).

       01  WRK-TAXA-ZD13-D2            PIC  9(13)V9(02)    VALUE ZEROS.
       01  FILLER REDEFINES WRK-TAXA-ZD13-D2.
           03  FILLER                  PIC X(008).
           03  WRK-TAXA-ZD05-D2        PIC 9(005)V9(02).

       01  WRK-TXA-JUROS               PIC 9(05)V9999      VALUE ZEROS.
       01  WRK-TXA-JUROS-CONTR         PIC 9(05)V9999      VALUE ZEROS.

       01  WRK-PJURO-BLETO             PIC 9(05)V99   VALUE ZEROS
                                                           COMP-3.
       01  WRK-PJURO-CTA               PIC 9(05)V99   VALUE ZEROS
                                                           COMP-3.
       01  WRK-FATOR-JURO              PIC 9(05)V99   VALUE ZEROS
                                                           COMP-3.
       01  WRK-TXA-JUROS-CP3           PIC 9(05)V9999 VALUE ZEROS
                                                           COMP-3.

       01  WRK-FIM-CSR-RUCAB004        PIC  X(01)          VALUE SPACES.

       01  WRK-LANCAMENTO              PIC S9(03)          VALUE ZEROS.

       01  WRK-CALC-CET                PIC  S9(08)V9(07) COMP-3
                                                            VALUE ZEROS.
       01  WRK-HEXA-S8V7               PIC  +9(08)V9(07)    VALUE ZEROS.
       01  FILLER                  REDEFINES WRK-HEXA-S8V7.
           03  WRK-HEXA-SINAL          PIC   X(01).
           03  FILLER                  PIC   X(06).
           03  WRK-HEXA-2V2            PIC   9(02)V9(02).
           03  FILLER                  PIC   X(05).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA DA TABELA   ***'.
      *----------------------------------------------------------------*

       01  WRK-LIBERACOES.
           03 WRK-LIB-TABELA.
              05 WRK-LIB-OCORR             OCCURS 050 TIMES.
                 07 WRK-LIB-DATA-L         PIC X(10)       VALUE SPACES.
                 07 WRK-LIB-CET            PIC S9(03)V99   VALUE ZEROS.
                 07 WRK-LIB-CET-CONTRATO   PIC S9(03)V99   VALUE ZEROS.

      *--- AREA DE COMUNICACAO POOL1080  ------------------------------*

       01  WRK-IMPRESSORA              PIC X(08)     VALUE SPACES.

      *--------------------------------------------------------------*
      * AREA  PARA COMUNICACAO COM  O  PROGRAMA  RUCA6691 (IMPRESSAO)*
      * MAPA  DE IMPRESSAO RUCAP690                                  *
      *--------------------------------------------------------------*
      *
       01  WRK-AREA-RUCA6691.
           03 WRK-E-DADOS-IMP.
              05 WRK-E-IMPRESSORA-IMP  PIC  X(008)         VALUE SPACES.
              05 WRK-E-COD-USER-IMP    PIC  X(007)         VALUE SPACES.
              05 WRK-E-PAGINA-IMP      PIC  9(003)         VALUE ZEROS.
              05 WRK-E-DTBASE-IMP      PIC  X(010)         VALUE SPACES.
              05 WRK-E-NROPER-IMP.
                 07 WRK-E-NROPER-IMP-N PIC  9(009)         VALUE ZEROS.
              05 WRK-E-MDOPER-IMP      PIC  X(040)         VALUE SPACES.
              05 WRK-E-SDVATU-IMP      PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
              05 WRK-E-TXJANU-IMP      PIC  ZZZZ9,99       VALUE ZEROS.
              05 WRK-E-TPCELA-IMP      PIC  9(003)         VALUE ZEROS.
              05 WRK-E-TXJNOM-IMP      PIC  ZZZZ9,99       VALUE ZEROS.
              05 WRK-E-TPCREM-IMP      PIC  9(003)         VALUE ZEROS.
              05 WRK-E-TXJEFE-IMP      PIC  ZZZZ9,99       VALUE ZEROS.
              05 WRK-E-SIPGTO-IMP      PIC  X(018)         VALUE SPACES.
              05 WRK-E-DTULVE-IMP      PIC  X(010)         VALUE SPACES.
           03 WRK-S-COD-RETORNO-IMP    PIC  9(002)         VALUE ZEROS.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA DO RUEC8880 ***'.
      *----------------------------------------------------------------*
       01 WRK-BLOCO-RETORNO.
          05 WRK-S-COD-RETORNO           PIC 9(002)        VALUE ZEROS.
          05 WRK-S-MENSAGEM              PIC X(040)        VALUE SPACES.
       01 WRK-BLOCO-ENTRADA.
          05 WRK-E-BASE-CALCULO          PIC  9(03)        VALUE ZEROS.
          05 WRK-E-TXA-JUR-ANO           PIC  9(05)V9999   VALUE ZEROS.
          05 WRK-E-QTD-OCORRENCIAS       PIC  9(03)        VALUE ZEROS.
          05 WRK-E-OCORRENCIAS           OCCURS 200 TIMES.
             15 WRK-E-DATA               PIC  X(10).
             15 WRK-E-TP-EVENTO          PIC  X(01).
             15 WRK-E-VALOR              PIC S9(15)V9(02).
       01 WRK-BLOCO-SAIDA.
          05 WRK-S-CET-DIA               PIC S9(08)V9(7)   VALUE ZEROS.
          05 WRK-S-CET-MES               PIC S9(08)V9(7)   VALUE ZEROS.
          05 WRK-S-CET-ANO               PIC S9(08)V9(7)   VALUE ZEROS.
          05 WRK-S-QTD-OCORRENCIAS       PIC  9(03)        VALUE ZEROS.
          05 WRK-S-OCORRENCIAS           OCCURS 200 TIMES.
             15 WRK-S-CET-LIB            PIC S9(08)V9(7).
      *
       01 WRK-RUEC8880                   PIC  X(08) VALUE 'RUEC8880'.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA DA POOL5000         ***'.
      *----------------------------------------------------------------*

       01  WRK-GU                      PIC  X(004)         VALUE 'GU'.
       01  WRK-CHNG                    PIC  X(004)         VALUE 'CHNG'.
       01  WRK-ISRT                    PIC  X(004)         VALUE 'ISRT'.
       01  WRK-FUNCAO                  PIC  X(004)         VALUE SPACES.
       01  WRK-TELA                    PIC  X(008)         VALUE SPACES.
       01  WRK-NOVA-OPCAO              PIC  9(002)         VALUE ZEROS.
       01  WRK-OPCAO-ATUAL             PIC  9(002)         VALUE ZEROS.

       01  WRK-COD-USER-X.
           05 WRK-COD-USER             PIC  9(007)         VALUE ZEROS.

       01  WRK-COD-DEPTO.
           05 FILLER                   PIC  X(002)         VALUE SPACES.
           05 WRK-COD-DEPTO-N          PIC  9(004)         VALUE ZEROS.

       01  WRK-MENSAGEM.
           05 FILLER                   PIC  X(004)         VALUE SPACES.
           05 WRK-TRANSACAO            PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(1988)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA DE ERRO DA BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-BRAD0660.
           05  FILLER                  PIC  X(045)       VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           05  FILLER                  PIC  X(012)       VALUE
               '- RET.CODE ='.
           05  WRK-RETURN-CODE         PIC  9(002)       VALUE ZEROS.
           05  FILLER                  PIC  X(011)       VALUE
               ' - LOCAL ='.
           05  WRK-LOCAL-ERRO          PIC  9(002)       VALUE ZEROS.
           05  FILLER                  PIC  X(002)       VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA DA RUCAT690 - INPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-INPUT.
           03 FILLER                   PIC  X(018)         VALUE SPACES.
           03 WRK-I-PFK                PIC  X(001)         VALUE SPACES.
           03 WRK-I-SENHAS             PIC  X(037)         VALUE SPACES.
           03 WRK-I-NOPCAO             PIC  X(002)         VALUE SPACES.
           03 WRK-I-FASE               PIC  X(001)         VALUE SPACES.
           03 WRK-I-PAGIMP             PIC  9(003)         VALUE ZEROS.
           03 WRK-I-PAGINA             PIC  9(003)         VALUE ZEROS.
           03 WRK-I-FIM                PIC  X(001)         VALUE SPACES.
           03 WRK-I-DADOS.
              05 WRK-I-DTBASE          PIC  X(010)         VALUE SPACES.
              05 WRK-I-NROPER.
                 07 WRK-I-NROPER-N     PIC  9(009)         VALUE ZEROS.
              05 WRK-I-MDOPER          PIC  X(040)         VALUE SPACES.
              05 WRK-I-SDVATU          PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
              05 WRK-I-TXJANU          PIC  ZZZZ9,99       VALUE ZEROS.
              05 WRK-I-TPCELA          PIC  9(003)         VALUE ZEROS.
              05 WRK-I-TXJNOM          PIC  ZZZZ9,99       VALUE ZEROS.
              05 WRK-I-TPCREM          PIC  9(003)         VALUE ZEROS.
              05 WRK-I-TXJEFE          PIC  ZZZZ9,99       VALUE ZEROS.
              05 WRK-I-SIPGTO          PIC  X(018)         VALUE SPACES.
              05 WRK-I-DTULVE          PIC  X(010)         VALUE SPACES.
              05 WRK-I-OCORRENCIAS.
                 07 WRK-I-LINHA        OCCURS 09 TIMES.
                    10 WRK-I-PCELA.
                       15 WRK-I-PCELA-N   PIC ZZZZZZ9      VALUE ZEROS.
                    10 WRK-I-DVCTO        PIC X(010)       VALUE SPACES.
                    10 WRK-I-VLPRI.
                       15 WRK-I-VLPRI-N   PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
                    10 WRK-I-VLENC.
                       15 WRK-I-VLENC-N   PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
                    10 WRK-I-SITUA        PIC X(009)       VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA RUCAT690  - OUTPUT  ***'.
      *----------------------------------------------------------------*

       01  WRK-OUTPUT.
           05 WRK-O-LL                 PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-O-ZZ                 PIC S9(004) COMP    VALUE ZEROS.
           03 WRK-O-SENHAS             PIC  X(037)         VALUE SPACES.
           03 WRK-O-NOPCAO             PIC  X(002)         VALUE SPACES.
           03 WRK-O-FASE               PIC  X(001)         VALUE SPACES.
           03 WRK-O-PAGIMP             PIC  9(003)         VALUE ZEROS.
           03 WRK-O-PAGINA             PIC  9(003)         VALUE ZEROS.
           03 WRK-O-FIM                PIC  X(001)         VALUE SPACES.
           03 WRK-O-DADOS.
              05 WRK-O-DTBASE          PIC  X(010)         VALUE SPACES.
              05 WRK-O-NROPER          PIC  X(009)         VALUE SPACES.
              05 WRK-O-MDOPER          PIC  X(040)         VALUE SPACES.
              05 WRK-O-SDVATU          PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
              05 WRK-O-TXJANU          PIC  ZZZZ9,99       VALUE ZEROS.
              05 WRK-O-TPCELA          PIC  9(003)         VALUE ZEROS.
              05 WRK-O-TXJNOM          PIC  ZZZZ9,99       VALUE ZEROS.
              05 WRK-O-TPCREM          PIC  9(003)         VALUE ZEROS.
              05 WRK-O-TXJEFE          PIC  ZZZZ9,99       VALUE ZEROS.
              05 WRK-O-SIPGTO          PIC  X(018)         VALUE SPACES.
              05 WRK-O-DTULVE          PIC  X(010)         VALUE SPACES.
              05 WRK-O-OCORRENCIAS.
                 07 WRK-O-LINHA        OCCURS 09 TIMES.
                    10 WRK-O-PCELA.
                       15 WRK-O-PCELA-N   PIC ZZZZZZ9      VALUE ZEROS.
                    10 WRK-O-DVCTO        PIC X(010)       VALUE SPACES.
                    10 WRK-O-VLPRI.
                       15 WRK-O-VLPRI-N   PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
                    10 WRK-O-VLENC.
                       15 WRK-O-VLENC-N   PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
                    10 WRK-O-SITUA        PIC X(009)       VALUE SPACES.
              05 WRK-O-MENSA           PIC  X(079)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA RUCAT690 - BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-660-RUCAT690.
           03 WRK-660-LL                 PIC S9(004) COMP  VALUE 0000.
           03 WRK-660-LL-OUTPUT          PIC S9(004) COMP  VALUE 0000.
           03 WRK-660-SENHAS             PIC S9(004) COMP  VALUE 0037.
           03 WRK-660-NOPCAO             PIC S9(004) COMP  VALUE 0002.
           03 WRK-660-FASE               PIC S9(004) COMP  VALUE 0001.
           03 WRK-660-PAGIMP             PIC S9(004) COMP  VALUE 0003.
           03 WRK-660-PAGINA             PIC S9(004) COMP  VALUE 0003.
           03 WRK-660-FIM                PIC S9(004) COMP  VALUE 0001.
           03 WRK-660-DTBASE             PIC S9(004) COMP  VALUE 0010.
           03 WRK-660-NROPER             PIC S9(004) COMP  VALUE 0009.
           03 WRK-660-MDOPER             PIC S9(004) COMP  VALUE 0040.
           03 WRK-660-SDVATU             PIC S9(004) COMP  VALUE 0022.
           03 WRK-660-TXJANU             PIC S9(004) COMP  VALUE 0008.
           03 WRK-660-TPCELA             PIC S9(004) COMP  VALUE 0003.
           03 WRK-660-TXJNOM             PIC S9(004) COMP  VALUE 0008.
           03 WRK-660-TPCREM             PIC S9(004) COMP  VALUE 0003.
           03 WRK-660-TXJEFE             PIC S9(004) COMP  VALUE 0008.
           03 WRK-660-SIPGTO             PIC S9(004) COMP  VALUE 0018.
           03 WRK-660-DTULVE             PIC S9(004) COMP  VALUE 0010.
           03 WRK-660-LINHA              OCCURS 09 TIMES.
              05 WRK-660-PCELA           PIC S9(004) COMP  VALUE 0007.
              05 WRK-660-DVCTO           PIC S9(004) COMP  VALUE 0010.
              05 WRK-660-VLPRI           PIC S9(004) COMP  VALUE 0022.
              05 WRK-660-VLENC           PIC S9(004) COMP  VALUE 0022.
              05 WRK-660-SITUA           PIC S9(004) COMP  VALUE 0009.
           03 WRK-660-MENSA              PIC S9(004) COMP  VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA DE COMUNICACAO      ***'.
      *----------------------------------------------------------------*

       01  WRK-COMU-AREA.
           05  WRK-COMU-LL             PIC S9(004) COMP  VALUE ZEROS.
           05  WRK-COMU-ZZ             PIC S9(004) COMP  VALUE ZEROS.
           05  WRK-COMU-TRANCODE.
               10  WRK-COMU-TRANSACAO  PIC  X(008)       VALUE SPACES.
               10  FILLER              PIC  X(006)       VALUE SPACES.
               10  WRK-COMU-PFK        PIC  X(001)       VALUE SPACES.
           05  WRK-COMU-SENHAS         PIC  X(037)       VALUE SPACES.
           05  WRK-COMU-OPCAO          PIC  X(002)       VALUE SPACES.
           05  WRK-COMU-NROPER         PIC  X(009)       VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA DA BRAD7100         ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS DE NULIDADE ***'.
      *----------------------------------------------------------------*

       77  WRK-DTEVENTO-NULL           PIC S9(004) COMP    VALUE ZEROS.
       77  WRK-CATVDD-RURAL-NULL       PIC S9(004) COMP    VALUE ZEROS.
       77  WRK-CFNALD-RURAL-NULL       PIC S9(004) COMP    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA PARA DB2            ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** INCLUDE SQLCA            ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE RUCAB003
           END-EXEC.

           EXEC SQL
               INCLUDE RUCAB004
           END-EXEC.

           EXEC SQL
               INCLUDE RUCAB022
           END-EXEC.

           EXEC SQL
               INCLUDE RUFIB002
           END-EXEC.

           EXEC SQL
               INCLUDE RUMOV000
           END-EXEC.

           EXEC SQL
               INCLUDE RUMOB002
           END-EXEC.

           EXEC SQL
               INCLUDE RURCV003
           END-EXEC.

           EXEC SQL
               INCLUDE RURCV004
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** CURSOR RUCAB004          ***'.
      *----------------------------------------------------------------*

      *-CURSOR 1 - RUCAB004--------------------------------------------*

           EXEC SQL
             DECLARE CSR01-RUCAB004 CURSOR FOR
             SELECT
               VALUE ( B004_DTCONTAB, B004_DTEVENTO )
             , B004_CODLANC
             , B004_DEBCRED
             , VALUE ( SUM ( B004_VLPRICOR ) , 0 )
             FROM  DB2PRD.RUCA_CAD_DEP
             WHERE B004_CH_CONTR   = :RUCAB004.B004-CH-CONTR
             AND   B004_DTACERT   IS NULL
             AND  ((B004_CODLANC  <>  58
             AND    B004_CODLANC  <>  59
             AND    B004_CODLANC  <= 60
             AND    B004_DEBCRED   = '2'
             AND    B004_VLPRICOR <>  0)
             OR    (B004_CODLANC   <  50
             AND    B004_CODLANC  <>  22
             AND    B004_CODLANC  <>  23
             AND    B004_CODLANC  <>  24
             AND    B004_CODLANC  <>  35
             AND    B004_CODLANC  <>  40
             AND    B004_CODLANC  <>  45
             AND    B004_CODLANC  <>  46
             AND    B004_CODLANC  <>  47
             AND    B004_DEBCRED   = '6'
             AND    B004_VLPRICOR <>  0))
             GROUP BY
               VALUE ( B004_DTCONTAB, B004_DTEVENTO )
             , B004_CODLANC
             , B004_DEBCRED
             ORDER BY
               1
           END-EXEC.

      *-CURSOR 2 - RUCAB004--------------------------------------------*
           EXEC SQL
             DECLARE  CSR02-RUCAB004 CURSOR WITH HOLD FOR
              SELECT  B004_CH_NUMERO
                   ,  B004_DTEVENTO
                   ,  B004_VLPRICOR
                   ,  B004_JURCORRI
                   ,  B004_CH_TIPO
                   ,  B004_CONTROLE
                FROM  DB2PRD.RUCA_CAD_DEP
               WHERE  B004_CH_CONTR = :RUCAB004.B004-CH-CONTR
                 AND  B004_VLPRICOR > :RUCAB004.B004-VLPRICOR
                 AND  B004_DEBCRED  = :RUCAB004.B004-DEBCRED
                 AND  B004_CODLANC  < :RUCAB004.B004-CODLANC
            ORDER BY  B004_DTEVENTO
                    , B004_CH_CONTR
                    , B004_CH_TIPO
                    , B004_CH_NUMERO
           END-EXEC.

      *-CURSOR 3 - RUCAB004--------------------------------------------*
           EXEC SQL
               DECLARE  CSR03-RUCAB004 CURSOR FOR
                SELECT  B004_CONTROLE
                  FROM  DB2PRD.RUCA_CAD_DEP
                 WHERE  B004_CH_CONTR      = :RUCAB004.B004-CH-CONTR
                   AND  B004_VLPRICOR      > :RUCAB004.B004-VLPRICOR
                   AND  B004_DEBCRED       = :RUCAB004.B004-DEBCRED
                   AND  B004_CODLANC       < :RUCAB004.B004-CODLANC
                   AND  B004_CH_TIPO      >= :RUCAB004.B004-CH-TIPO
                   AND  B004_CH_NUMERO    >= :RUCAB004.B004-CH-NUMERO
MJ0001             AND  B004_DTEVENTO     >= CURRENT DATE
                 ORDER  BY  B004_CH_CONTR ,
                            B004_CH_TIPO  ,
                            B004_CH_NUMERO
           END-EXEC.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '** RUCA6690 - FIM DA WORKING  **'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-IO-PCB.
           05  LNK-IO-LTERM            PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  LNK-IO-STATUS           PIC  X(002).
           05  FILLER                  PIC  X(012).
           05  LNK-IO-MODNAME          PIC  X(008).

       01  LNK-ALT-PCB.
           05  LNK-ALT-LTERM           PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  LNK-ALT-STATUS          PIC  X(002).
           05  FILLER                  PIC  X(012).
           05  LNK-ALT-MODNAME         PIC  X(008).


      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           ENTRY  'DLITCBL'            USING  LNK-IO-PCB
                                              LNK-ALT-PCB.
           MOVE  WRK-GU                TO  WRK-FUNCAO.

      *-----------------*
       0000-10-POOL5000.
      *-----------------*

           CALL 'POOL5000'             USING WRK-FUNCAO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             WRK-MENSAGEM
                                             WRK-TELA
                                             WRK-NOVA-OPCAO
                                             WRK-COD-USER
                                             WRK-OPCAO-ATUAL
                                             WRK-COD-DEPTO.

           IF  RETURN-CODE          EQUAL  04 OR 12
               PERFORM 8000-FINALIZAR
           END-IF.

           MOVE SPACES                 TO WRK-I-DADOS
                                          WRK-O-DADOS.

           PERFORM 1000-PROCESSAR-MENSAGEM.

           GO TO 0000-10-POOL5000.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE  'RUCAT690'            TO  WRK-TELA.

           IF WRK-TRANSACAO            NOT EQUAL  'RUCA6690'
              PERFORM 2000-RECEBER-CONTROLE
           ELSE
              PERFORM 3000-PROCESSAR-RUCAT690
           END-IF.

           IF WRK-TELA              EQUAL  'RUCAT690'
              PERFORM 7000-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO WRK-COMU-AREA.
           MOVE WRK-COMU-NROPER        TO WRK-I-NROPER.
           MOVE ZEROS                  TO WRK-I-PAGIMP.
           MOVE 001                    TO WRK-I-PAGINA.
           MOVE '1'                    TO WRK-I-FASE.

      *---DADOS FIXOS NA TELA------------------------------------------*
           EXEC SQL
                SET :WRK-DATA-CORRENTE = CURRENT DATE - 1 DAY
           END-EXEC.

           INSPECT WRK-DATA-CORRENTE   REPLACING ALL '.' BY '/'
           MOVE WRK-DATA-CORRENTE      TO WRK-I-DTBASE.

           PERFORM 3400-BUSCAR-MODALIDADE-OPER.
           MOVE WRK-MDOPER             TO WRK-I-MDOPER.

           PERFORM 3900-VERIFICA-CTR-LP

           IF  WRK-CTR-LP              EQUAL 'S'
               PERFORM 3200-DEVOLVER-TELA
               GO                      TO 2000-99-FIM
           END-IF

           PERFORM 3500-CALCULAR-SALDO-DEVATU.
           MOVE WRK-ACU-SDVATU         TO WRK-VALOR-S13-D2.
           MOVE WRK-VALOR-Z13-D2       TO WRK-I-SDVATU.

           PERFORM 3600-CALCULAR-TOTAL-PCELA.
           MOVE WRK-COUNT              TO WRK-NUM-09-CS.
           MOVE WRK-NUM-09-SS          TO WRK-NRO-09.
           MOVE WRK-NRO-03             TO WRK-I-TPCELA.

           PERFORM 3800-CALCULAR-TOTAL-PCREM.
           MOVE WRK-ACU-TPCREM         TO WRK-NUM-09-CS.
           MOVE WRK-NUM-09-SS          TO WRK-NRO-09.
           MOVE WRK-NRO-03             TO WRK-I-TPCREM.
           IF  WRK-ACU-TPCREM          GREATER ZEROS
MJ0001         COMPUTE WRK-VALOR-JUR-AUX = B003-JURPGAC-PAG8
                                     / WRK-ACU-TPCREM
           ELSE
               MOVE B003-JURPGAC-PAG8  TO WRK-VALOR-JUR-AUX
           END-IF

           PERFORM 3700-OBTER-TAXA-JUROS.
           MOVE WRK-TAXA-ZD05-D2       TO WRK-I-TXJANU
                                          WRK-I-TXJNOM.
           MOVE WRK-LIB-CET-CONTRATO(1)
                                       TO WRK-I-TXJEFE.

           MOVE 'PRE-FIXADO'           TO WRK-I-SIPGTO.

           MOVE DVCTO-CONTR            OF RUFIB002
                                       TO WRK-DATA-DB2.
           MOVE WRK-DD-DB2             TO WRK-DD
           MOVE WRK-MM-DB2             TO WRK-MM
           MOVE WRK-AAAA-DB2           TO WRK-AAAA.
           MOVE WRK-DATA-BARRA         TO WRK-I-DTULVE.

      *---DADOS DE PAGINACAO-------------------------------------------*
           PERFORM 5000-MONTAR-LISTA.

           PERFORM 3200-DEVOLVER-TELA.

           MOVE 'CONSULTA EFETUADA'    TO WRK-O-MENSA.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR-RUCAT690         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO  WRK-INPUT.

           PERFORM 3100-EFETUAR-INSPECT.
           PERFORM 3200-DEVOLVER-TELA.

           EVALUATE  TRUE

               WHEN  WRK-I-PFK            EQUAL  'C'
                     PERFORM 3300-RETORNAR-CHAMADOR

               WHEN  WRK-I-PFK            EQUAL  '4'
                     PERFORM 3900-VERIFICA-CTR-LP
                     IF  WRK-CTR-LP    EQUAL 'N'
                         PERFORM 5700-TRATAR-PF4
                     END-IF

               WHEN  WRK-I-PFK            EQUAL  '7'
                     PERFORM 3900-VERIFICA-CTR-LP
                     IF  WRK-CTR-LP    EQUAL 'N'
                         PERFORM 5500-TRATAR-PF7
                     END-IF

               WHEN  WRK-I-PFK            EQUAL  '8'
                     PERFORM 3900-VERIFICA-CTR-LP
                     IF  WRK-CTR-LP    EQUAL 'N'
                         PERFORM 5600-TRATAR-PF8
                     END-IF

               WHEN  OTHER
                     MOVE 'PF INVALIDA'   TO WRK-O-MENSA

           END-EVALUATE.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-EFETUAR-INSPECT            SECTION.
      *----------------------------------------------------------------*

           IF  WRK-I-NROPER            NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-I-NROPER    REPLACING LEADING ' ' BY '0'
           END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3200-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-I-SENHAS           TO WRK-O-SENHAS
           MOVE WRK-I-FASE             TO WRK-O-FASE
           MOVE WRK-I-PAGIMP           TO WRK-O-PAGIMP
           MOVE WRK-I-PAGINA           TO WRK-O-PAGINA
           MOVE WRK-I-FIM              TO WRK-O-FIM
           MOVE WRK-I-DTBASE           TO WRK-O-DTBASE
           MOVE WRK-I-NROPER           TO WRK-O-NROPER
           MOVE WRK-I-MDOPER           TO WRK-O-MDOPER
           MOVE WRK-I-SDVATU           TO WRK-O-SDVATU
           MOVE WRK-I-TXJANU           TO WRK-O-TXJANU
           MOVE WRK-I-TPCELA           TO WRK-O-TPCELA
           MOVE WRK-I-TXJNOM           TO WRK-O-TXJNOM
           MOVE WRK-I-TPCREM           TO WRK-O-TPCREM
           MOVE WRK-I-TXJEFE           TO WRK-O-TXJEFE
           MOVE WRK-I-SIPGTO           TO WRK-O-SIPGTO
           MOVE WRK-I-DTULVE           TO WRK-O-DTULVE.

           PERFORM VARYING IND-1       FROM  1 BY 1
                   UNTIL   IND-1       GREATER 09
              MOVE WRK-I-PCELA(IND-1)  TO WRK-O-PCELA(IND-1)
              MOVE WRK-I-DVCTO(IND-1)  TO WRK-O-DVCTO(IND-1)
              MOVE WRK-I-VLPRI(IND-1)  TO WRK-O-VLPRI(IND-1)
              MOVE WRK-I-VLENC(IND-1)  TO WRK-O-VLENC(IND-1)
              MOVE WRK-I-SITUA(IND-1)  TO WRK-O-SITUA(IND-1)
           END-PERFORM.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3300-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE  WRK-COMU-AREA.

           MOVE WRK-CHNG               TO WRK-FUNCAO
           MOVE LENGTH OF WRK-COMU-AREA TO WRK-COMU-LL.
           MOVE 'RUCA6630'             TO WRK-TELA
           MOVE 'RUCA6690'             TO WRK-COMU-TRANSACAO
           MOVE WRK-I-SENHAS           TO WRK-COMU-SENHAS.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3400-BUSCAR-MODALIDADE-OPER     SECTION.
      *----------------------------------------------------------------*

      *---BUSCA O CODIGO DA ATIVIDADE----------------------------------*
           MOVE WRK-I-NROPER-N        TO CCONTR             OF RUFIB002.

           EXEC SQL
                SELECT  CATVDD_RURAL
                       ,CFNALD_RURAL
                       ,DVCTO_CONTR
                  INTO :RUFIB002.CATVDD-RURAL
                            :WRK-CATVDD-RURAL-NULL
                      ,:RUFIB002.CFNALD-RURAL
                            :WRK-CFNALD-RURAL-NULL
                      ,:RUFIB002.DVCTO-CONTR
                  FROM  DB2PRD.CONTRATO_RURAL
                 WHERE  CCONTR = :RUFIB002.CCONTR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W'               )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUFIB002'         TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '3400'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE SPACES             TO WRK-I-MDOPER
               GO                      TO 3400-99-FIM
           END-IF.

           IF  WRK-CATVDD-RURAL-NULL   NOT EQUAL ZEROS
               MOVE ZEROS              TO CATVDD-RURAL
                                       OF RUFIB002
           END-IF.

           IF  WRK-CFNALD-RURAL-NULL   NOT EQUAL ZEROS
               MOVE ZEROS              TO CFNALD-RURAL
                                       OF RUFIB002
           END-IF.

      *---BUSCA A DESCRICAO DA ATIVIDADE-------------------------------*
           MOVE CATVDD-RURAL          OF RUFIB002
                                      TO CATVDD-RURAL       OF RURCV004.

           EXEC SQL
                SELECT  IATVDD_RURAL
                  INTO :RURCV004.IATVDD-RURAL
                  FROM  DB2PRD.V01ATIVIDADE_RURAL
                 WHERE  CATVDD_RURAL  = :RURCV004.CATVDD-RURAL
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W'               )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RURCV004'         TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '3401'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE SPACES             TO WRK-MDOPER
           ELSE
               MOVE IATVDD-RURAL       OF RURCV004
                                       TO WRK-IATVDD-RURAL
           END-IF.

      *---BUSCA A DESCRICAO DA FINALIDADE------------------------------*
           MOVE CFNALD-RURAL          OF RUFIB002
                                      TO CFNALD-RURAL       OF RURCV003.

           EXEC SQL
                SELECT  IFNALD_RURAL
                  INTO :RURCV003.IFNALD-RURAL
                  FROM  DB2PRD.V01FNALD_RURAL
                 WHERE  CFNALD_RURAL  = :RURCV003.CFNALD-RURAL
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W'               )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RURCV003'         TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '3402'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE SPACES             TO WRK-IFNALD-RURAL
           ELSE
               IF  WRK-IATVDD-RURAL    EQUAL SPACES
                   MOVE IFNALD-RURAL   OF RURCV003
                                       TO WRK-IATVDD-RURAL
                   MOVE SPACES         TO WRK-IFNALD-RURAL
               ELSE
                   MOVE IFNALD-RURAL   OF RURCV003
                                       TO WRK-IFNALD-RURAL
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3500-CALCULAR-SALDO-DEVATU      SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO B003-VLAUXIL-OPER9
                                                           OF RUCAB003.
           MOVE ZEROS                  TO WRK-ACU-SDVATU.
           MOVE WRK-I-NROPER-N         TO B003-CH-CONTR    OF RUCAB003.

           EXEC SQL
                SELECT B003_VLAUXIL_OPER9
                    + (B003_JURPGAC_PAG8  -  B003_JURPGORI_CAP9 )
                    + (B003_CORRPGAC_PAG8 -  B003_CORRPGOR_CAP9 )
MJ0001               ,(B003_JURPGAC_PAG8  -  B003_JURPGORI_CAP9 )
MJ0001               ,B003_JURPGORI_CAP9
                INTO  :RUCAB003.B003-VLAUXIL-OPER9
MJ0001               ,:RUCAB003.B003-JURPGAC-PAG8
MJ0001               ,:RUCAB003.B003-JURPGORI-CAP9
                FROM   DB2PRD.RUCA_CAD_ROOT
                WHERE  B003_CH_CONTR      = :RUCAB003.B003-CH-CONTR
           END-EXEC

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W'               )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUCAB003'         TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '3500'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE EQUAL ZEROS
               MOVE B003-VLAUXIL-OPER9 OF RUCAB003
                                       TO WRK-ACU-SDVATU
           END-IF.

           MOVE ZEROS                  TO VPCELA           OF RUMOV000.
           MOVE WRK-I-NROPER-N         TO CCONTR           OF RUMOV000.

           EXEC SQL
                SELECT VALUE ( SUM(VPCELA) +  SUM(VPROVS_JURO) , 0 )
                INTO  :RUMOV000.VPCELA
                FROM   DB2PRD.V01PARCELA_PGTO
                WHERE  CCONTR              = :RUMOV000.CCONTR
           END-EXEC

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W'               )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUMOV000'         TO ERR-DBD-TAB
               MOVE 'SUM'              TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '3501'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE EQUAL ZEROS
               COMPUTE WRK-ACU-SDVATU  =  WRK-ACU-SDVATU + VPCELA
                                                           OF RUMOV000
           END-IF.

      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3600-CALCULAR-TOTAL-PCELA       SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO WRK-COUNT.
           MOVE WRK-I-NROPER-N         TO B004-CH-CONTR    OF RUCAB004.
           MOVE 0                      TO B004-VLPRICOR    OF RUCAB004.
           MOVE '6'                    TO B004-DEBCRED     OF RUCAB004.
           MOVE 23                     TO B004-CODLANC     OF RUCAB004.

           EXEC SQL
                SELECT COUNT(*)
                INTO  :WRK-COUNT
                FROM   DB2PRD.RUCA_CAD_DEP
                WHERE  B004_CH_CONTR = :RUCAB004.B004-CH-CONTR
                  AND  B004_VLPRICOR > :RUCAB004.B004-VLPRICOR
                  AND  B004_DEBCRED  = :RUCAB004.B004-DEBCRED
                  AND  B004_CODLANC  < :RUCAB004.B004-CODLANC
           END-EXEC

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W'               )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUCAB004'         TO ERR-DBD-TAB
               MOVE 'COUNT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '3600'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3700-OBTER-TAXA-JUROS           SECTION.
      *----------------------------------------------------------------*

           PERFORM 3710-OBTER-TXJUR-ANUAL-NOMINAL.

           PERFORM 3720-OBTER-TXJUR-EFETIVA.

      *----------------------------------------------------------------*
       3700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3710-OBTER-TXJUR-ANUAL-NOMINAL  SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-I-NROPER-N         TO B004-CH-CONTR    OF RUCAB004.

           EXEC SQL
                SELECT  B004_VLAUXIL
                       ,B004_CODLANC
                INTO   :RUCAB004.B004-VLAUXIL
                      ,:RUCAB004.B004-CODLANC
                FROM    DB2PRD.RUCA_CAD_DEP
                WHERE   B004_CH_CONTR         =:RUCAB004.B004-CH-CONTR
                AND     B004_CODLANC         >= 200
                AND     B004_CODLANC          < 300
                AND     B004_DEBCRED          = '8'
                ORDER BY
                        B004_CH_NUMERO DESC
                       ,B004_DTEVENTO  DESC
                FETCH FIRST 1 ROW ONLY
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUCAB004'         TO ERR-DBD-TAB
               MOVE 'COUNT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '3710'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE ZEROS              TO B004-CODLANC       OF RUCAB004
                                          B004-VLAUXIL       OF RUCAB004
           END-IF.

           MOVE B004-CODLANC           OF RUCAB004
                                       TO WRK-LANCAMENTO
           MOVE B004-VLAUXIL           OF RUCAB004
                                       TO WRK-VALOR-S13-D2
           MOVE WRK-VALOR-Z13-D2       TO WRK-TAXA-ZD13-D2.
           MOVE WRK-TAXA-ZD05-D2       TO WRK-TXA-JUROS
                                          WRK-TXA-JUROS-CONTR.

      *----------------------------------------------------------------*
       3710-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3720-OBTER-TXJUR-EFETIVA        SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-I-NROPER-N         TO B004-CH-CONTR    OF RUCAB004.

           PERFORM 3721-ABRIR-CSR01-RUCAB004.

           PERFORM 3722-LER-CSR01-RUCAB004.

           PERFORM VARYING  IND-HEX FROM  1 BY 1
             UNTIL IND-HEX          GREATER 50
             MOVE LOW-VALUES        TO WRK-LIB-OCORR(IND-HEX)
             MOVE ZEROS             TO WRK-LIB-CET  (IND-HEX)
                                       WRK-LIB-CET-CONTRATO(IND-HEX)
           END-PERFORM.

           MOVE ZERO                  TO IND-HEX

           PERFORM 3723-PROCES-CSR-RUCAB004
               UNTIL WRK-FIM-CSR-RUCAB004 EQUAL 'S'

           PERFORM 3724-OBTER-FORMA-PGTO.

           PERFORM 3725-OBTER-FATOR-REDUTOR.

           PERFORM 3726-OBTER-TX-DEBITO.

           PERFORM 3728-ACESSAR-RUEC8880.

           PERFORM VARYING  IND-HEX2  FROM  1 BY 1
             UNTIL IND-HEX2           GREATER 50
              MOVE WRK-LIB-CET(IND-HEX2)
                                      TO WRK-LIB-CET-CONTRATO(IND-HEX2)
           END-PERFORM.

           IF WRK-FATOR-JURO          NOT EQUAL ZEROS
              PERFORM VARYING IND-HEX2 FROM  1 BY 1
                UNTIL IND-HEX2         GREATER 50
                 MOVE ZEROS            TO WRK-LIB-CET(IND-HEX2)
              END-PERFORM

              PERFORM 3727-OBTER-TX-BLETO

              PERFORM 3728-ACESSAR-RUEC8880
           END-IF.

           PERFORM 3730-FECHAR-CSR01-RUCAB004.

      *----------------------------------------------------------------*
       3720-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3721-ABRIR-CSR01-RUCAB004        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
                 OPEN   CSR01-RUCAB004
           END-EXEC.

           IF ( SQLCODE                NOT EQUAL ZEROS )   OR
              ( SQLWARN0               EQUAL 'W'   )
                MOVE 'DB2'                TO ERR-TIPO-ACESSO
                MOVE 'RUCAB004'           TO ERR-DBD-TAB
                MOVE 'OPEN    '           TO ERR-FUN-COMANDO
                MOVE SQLCODE              TO ERR-SQL-CODE
                MOVE '3721'               TO ERR-LOCAL
                MOVE SPACES               TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE 'N'                    TO WRK-FIM-CSR-RUCAB004.

      *---------------------------------------------------------------*
       3721-99-FIM.                   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3722-LER-CSR01-RUCAB004          SECTION.
      *---------------------------------------------------------------*

           MOVE ZEROS                  TO WRK-DTEVENTO-NULL

           EXEC SQL
             FETCH CSR01-RUCAB004
             INTO
               :RUCAB004.B004-DTEVENTO
                    :WRK-DTEVENTO-NULL
             , :RUCAB004.B004-CODLANC
             , :RUCAB004.B004-DEBCRED
             , :RUCAB004.B004-VLPRICOR
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100)   OR
               ( SQLWARN0              EQUAL 'W' )
                 MOVE 'DB2'                TO ERR-TIPO-ACESSO
                 MOVE 'RUCAB004'           TO ERR-DBD-TAB
                 MOVE 'FETCH   '           TO ERR-FUN-COMANDO
                 MOVE SQLCODE              TO ERR-SQL-CODE
                 MOVE '3722'               TO ERR-LOCAL
                 MOVE SPACES               TO ERR-SEGM
                 PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF SQLCODE                  EQUAL +100
              MOVE 'S'                 TO WRK-FIM-CSR-RUCAB004
              GO TO 3722-99-FIM
           END-IF.

           IF WRK-DTEVENTO-NULL        LESS ZEROS
              MOVE SPACES              TO B004-DTEVENTO OF RUCAB004
           END-IF.

      *---------------------------------------------------------------*
       3722-99-FIM.                   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3723-PROCES-CSR-RUCAB004       SECTION.
      *---------------------------------------------------------------*

           ADD 1                   TO IND-HEX

           IF B004-DEBCRED         OF RUCAB004 EQUAL '2'
              MOVE B004-DTEVENTO   OF RUCAB004
                                   TO WRK-LIB-DATA-L(IND-HEX)
                                      WRK-E-DATA (IND-HEX)
              INSPECT WRK-LIB-DATA-L(IND-HEX)
                                       REPLACING ALL '.' BY '/'
              MOVE 'L'             TO WRK-E-TP-EVENTO(IND-HEX)
              MOVE B004-VLPRICOR   OF RUCAB004
                                   TO WRK-E-VALOR(IND-HEX)
           ELSE
              MOVE B004-DTEVENTO   OF RUCAB004
                                   TO WRK-E-DATA (IND-HEX)
              MOVE 'A'             TO WRK-E-TP-EVENTO(IND-HEX)
              MOVE B004-VLPRICOR   OF RUCAB004
                                   TO WRK-E-VALOR(IND-HEX)
              MOVE SPACES          TO WRK-LIB-DATA-L(IND-HEX)
           END-IF.
           PERFORM 3722-LER-CSR01-RUCAB004.

      *---------------------------------------------------------------*
       3723-99-FIM.                   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3724-OBTER-FORMA-PGTO           SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-I-NROPER-N         TO CCONTR           OF RUFIB002.

           EXEC SQL
             SELECT
               VALUE( CFORMA_LIQDC , 0 )
             INTO
               :RUFIB002.CFORMA-LIQDC
             FROM DB2PRD.CONTRATO_RURAL
             WHERE
               CCONTR = :RUFIB002.CCONTR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUFIB002'         TO ERR-DBD-TAB
               MOVE SQLWARN            TO ERR-DBD-TAB (9:8)
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '3724'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *--CFORMA-LIQDC - RUFIB002
      *--        = 03 - BOLETO
      *--        = 06 - DEBITO C/C
      *--        <>   - INVALIDO

      *----------------------------------------------------------------*
       3724-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3725-OBTER-FATOR-REDUTOR        SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-I-NROPER-N         TO CCONTR-CREDT-RURAL OF RUCAB022

           EXEC SQL
             SELECT PJURO_DEB_BLETO,
                    PJURO_DEB_CTA
             INTO   :RUCAB022.PJURO-DEB-BLETO,
                    :RUCAB022.PJURO-DEB-CTA
             FROM  DB2PRD.TLOG_ENCAR_CONTR
             WHERE CCONTR_CREDT_RURAL    = :RUCAB022.CCONTR-CREDT-RURAL
             ORDER BY HINCL_REG
             FETCH FIRST 1 ROW ONLY
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'TLOG_ENCAR_CONTR' TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '3725'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE ZEROS              TO WRK-FATOR-JURO
           ELSE
               MOVE PJURO-DEB-BLETO    OF RUCAB022
                                       TO WRK-PJURO-BLETO

               MOVE PJURO-DEB-CTA      OF RUCAB022
                                       TO WRK-PJURO-CTA

               COMPUTE WRK-FATOR-JURO = WRK-PJURO-BLETO -
                                        WRK-PJURO-CTA
           END-IF.

      *----------------------------------------------------------------*
       3725-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3726-OBTER-TX-DEBITO            SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-TXA-JUROS-CONTR    TO WRK-TXA-JUROS-CP3

           EVALUATE CFORMA-LIQDC OF RUFIB002
               WHEN 03
                    COMPUTE WRK-TXA-JUROS-CP3 = WRK-TXA-JUROS-CP3 -
                                                WRK-FATOR-JURO
                    MOVE WRK-TXA-JUROS-CP3
                                       TO WRK-TXA-JUROS

               WHEN OTHER
                    MOVE WRK-TXA-JUROS-CONTR
                                       TO WRK-TXA-JUROS
           END-EVALUATE.

      *----------------------------------------------------------------*
       3726-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3727-OBTER-TX-BLETO             SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-TXA-JUROS-CONTR    TO WRK-TXA-JUROS-CP3

           EVALUATE CFORMA-LIQDC OF RUFIB002
               WHEN 00
               WHEN 06
                    COMPUTE WRK-TXA-JUROS-CP3 = WRK-TXA-JUROS-CP3 +
                                                WRK-FATOR-JURO
                    MOVE WRK-TXA-JUROS-CP3
                                       TO WRK-TXA-JUROS

               WHEN OTHER
                    MOVE WRK-TXA-JUROS-CONTR
                                       TO WRK-TXA-JUROS
           END-EVALUATE.

      *----------------------------------------------------------------*
       3727-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3728-ACESSAR-RUEC8880            SECTION.
      *---------------------------------------------------------------*

           INITIALIZE WRK-BLOCO-RETORNO
                      WRK-BLOCO-SAIDA.

           MOVE IND-HEX               TO WRK-E-QTD-OCORRENCIAS

           IF WRK-LANCAMENTO          EQUAL 227
              MOVE 365                TO WRK-E-BASE-CALCULO
           ELSE
              MOVE 360                TO WRK-E-BASE-CALCULO
           END-IF.

           MOVE WRK-TXA-JUROS         TO WRK-E-TXA-JUR-ANO

           CALL WRK-RUEC8880           USING WRK-BLOCO-RETORNO
                                             WRK-BLOCO-ENTRADA
                                             WRK-BLOCO-SAIDA
                                             ERRO-AREA

           IF WRK-S-COD-RETORNO        NOT EQUAL ZEROS
              IF  WRK-S-COD-RETORNO    EQUAL 98
                  MOVE WRK-S-MENSAGEM  TO WRK-O-MENSA
              ELSE
                  MOVE 'ERRO AO OBTER CET'
                                       TO WRK-O-MENSA
              END-IF
           ELSE
              PERFORM 3729-PROCESSA-RUEC8880
           END-IF.

      *---------------------------------------------------------------*
       3728-99-FIM.                   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3729-PROCESSA-RUEC8880           SECTION.
      *---------------------------------------------------------------*

           PERFORM VARYING IND-3 FROM 1 BY 1
                     UNTIL IND-3 GREATER WRK-S-QTD-OCORRENCIAS

              IF WRK-LIB-DATA-L(IND-3) NOT EQUAL SPACES
                 MOVE WRK-S-CET-LIB(IND-3)
                                       TO WRK-CALC-CET
                 COMPUTE  WRK-CALC-CET = WRK-CALC-CET * 100
                 MOVE WRK-CALC-CET     TO WRK-HEXA-S8V7
                 MOVE WRK-HEXA-2V2     TO WRK-LIB-CET(IND-3)
              END-IF
           END-PERFORM.

      *---------------------------------------------------------------*
       3729-99-FIM.                   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3730-FECHAR-CSR01-RUCAB004       SECTION.
      *---------------------------------------------------------------*


           EXEC SQL
             CLOSE   CSR01-RUCAB004
           END-EXEC.

           IF ( SQLCODE                NOT EQUAL ZEROS )   OR
              ( SQLWARN0               EQUAL 'W' )
                MOVE 'DB2'                TO ERR-TIPO-ACESSO
                MOVE 'RUCA_CAD_DEP'       TO ERR-DBD-TAB
                MOVE 'CLOSE   '           TO ERR-FUN-COMANDO
                MOVE SQLCODE              TO ERR-SQL-CODE
                MOVE '3730'               TO ERR-LOCAL
                MOVE SPACES               TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3730-99-FIM.                   EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       3800-CALCULAR-TOTAL-PCREM       SECTION.
      *----------------------------------------------------------------*

           PERFORM 3810-OPEN-CSR03-RUCAB004

           PERFORM 3820-FETCH-CSR03-RUCAB004.

           PERFORM 3830-OBTER-PARCELAS.

           PERFORM 3840-CLOSE-CSR03-RUCAB004.

      *---------------------------------------------------------------*
       3800-99-FIM.                   EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       3810-OPEN-CSR03-RUCAB004        SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-I-NROPER-N         TO B004-CH-CONTR    OF RUCAB004.
           MOVE 0                      TO B004-VLPRICOR    OF RUCAB004.
           MOVE '6'                    TO B004-DEBCRED     OF RUCAB004.
           MOVE 23                     TO B004-CODLANC     OF RUCAB004.
           MOVE ZEROS                  TO B004-CH-TIPO     OF RUCAB004.
           MOVE 1                      TO B004-CH-NUMERO   OF RUCAB004.
           MOVE ZEROS                  TO WRK-ACU-TPCREM.

           EXEC SQL
                OPEN CSR03-RUCAB004
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS )
           OR (SQLWARN0                EQUAL 'W'       )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUCAB004'         TO ERR-DBD-TAB
               MOVE 'OPEN'             TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '3810'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3810-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3820-FETCH-CSR03-RUCAB004       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR03-RUCAB004
                 INTO :RUCAB004.B004-CONTROLE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W'               )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUCAB004'         TO ERR-DBD-TAB
               MOVE 'FETCH'            TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '3820'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3820-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3830-OBTER-PARCELAS             SECTION.
      *----------------------------------------------------------------*

           PERFORM
             UNTIL SQLCODE             EQUAL  +100
               IF  B004-CONTROLE       OF RUCAB004 (1:1)
                                       LESS    WRK-BYTE-LIGADO
                   COMPUTE WRK-ACU-TPCREM
                                       =  WRK-ACU-TPCREM  +  1
               END-IF

               PERFORM  3820-FETCH-CSR03-RUCAB004

           END-PERFORM.

      *----------------------------------------------------------------*
       3830-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3840-CLOSE-CSR03-RUCAB004       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR03-RUCAB004
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS )
           OR (SQLWARN0                EQUAL 'W'       )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUCAB004'         TO ERR-DBD-TAB
               MOVE 'CLOSE'            TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '3840'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3840-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3900-VERIFICA-CTR-LP            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE RUMOB002

           MOVE WRK-I-NROPER-N         TO CCONTR           OF RUMOB002
           MOVE 7180                   TO CTPO-EVNTO-RURAL OF RUMOB002

           EXEC SQL
                SELECT CCONTR
                INTO  :RUMOB002.CCONTR
                FROM   DB2PRD.V01HIST_MOVTO
                WHERE  CCONTR           = :RUMOB002.CCONTR
                  AND  CTPO_EVNTO_RURAL =  7180
                FETCH  FIRST 1 ROWS ONLY
           END-EXEC.

           IF (SQLCODE             NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W'       )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUMOB002'         TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '3900'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.


           IF  SQLCODE                 EQUAL ZEROS
               MOVE 'S'                TO WRK-CTR-LP
               MOVE 'CONTRATO TRANSFERIDO PARA LP. ENTRAR EM CONTATO COM
      -             ' O 4120-DRC'      TO WRK-O-MENSA
           ELSE
               MOVE 'N'                TO WRK-CTR-LP
           END-IF.

      *----------------------------------------------------------------*
       3900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5000-MONTAR-LISTA               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                SET :WRK-DATA-CORRENTE = CURRENT DATE
           END-EXEC.

           MOVE WRK-DATA-CORRENTE      TO WRK-DATA-DB2.
           MOVE WRK-DD-DB2             TO WRK-DD-CUR
           MOVE WRK-MM-DB2             TO WRK-MM-CUR
           MOVE WRK-AAAA-DB2           TO WRK-AAAA-CUR.

           PERFORM 5200-ABRIR-CSR02.

           MOVE  WRK-I-PAGINA          TO  WRK-PAGINA.
           COMPUTE WRK-DESPREZAR       EQUAL ( WRK-PAGINA - 1 ) * 09 + 1
           MOVE  WRK-DESPREZAR         TO  WRK-SEQ-PCELA.

           PERFORM 5300-LER-CSR02      VARYING IND-1 FROM 1 BY 1
                   UNTIL   IND-1       GREATER WRK-DESPREZAR
                   OR      WRK-FIM-CSR EQUAL 'S'.

           PERFORM VARYING IND-1       FROM 1 BY 1
                   UNTIL   IND-1       GREATER   09
                   OR      WRK-FIM-CSR EQUAL 'S'
               PERFORM 5100-MOVER-OCORRENCIA
               PERFORM 5300-LER-CSR02
           END-PERFORM.

           PERFORM VARYING IND-1       FROM IND-1 BY 1
                   UNTIL   IND-1       GREATER 09
               MOVE SPACES             TO  WRK-I-PCELA        (IND-1)
                                           WRK-I-DVCTO        (IND-1)
                                           WRK-I-VLPRI        (IND-1)
                                           WRK-I-VLENC        (IND-1)
                                           WRK-I-SITUA        (IND-1)
           END-PERFORM.

           MOVE  WRK-FIM-CSR           TO  WRK-I-FIM.

           PERFORM 5400-FECHAR-CSR02.

      *----------------------------------------------------------------*
       5000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5100-MOVER-OCORRENCIA           SECTION.
      *----------------------------------------------------------------*

           PERFORM 5111-VERIFICAR-RUMO

           MOVE WRK-SEQ-PCELA          TO WRK-I-PCELA-N     (IND-1)
           MOVE B004-DTEVENTO          OF RUCAB004
                                       TO WRK-I-DVCTO       (IND-1)
           INSPECT WRK-I-DVCTO(IND-1)  REPLACING ALL '.' BY '/'
           MOVE B004-VLPRICOR          OF RUCAB004
                                       TO WRK-VALOR-S13-D2
           MOVE WRK-VALOR-Z13-D2       TO WRK-I-VLPRI-N     (IND-1)

MJ0001******OVE B004-JURCORRI          OF RUCAB004

MJ0001     COMPUTE WRK-VLJUR-RUMO      = WRK-VLJUR-RUMO
MJ0001                                 + B004-JURCORRI OF RUCAB004

           PERFORM 5110-FORMATAR-SITUACAO.

           IF  WRK-VLJUR-RUMO          GREATER ZEROS
             IF WRK-I-SITUA(IND-1)     EQUAL 'A VENCER'
               COMPUTE WRK-VLJUR-RUMO = WRK-VLJUR-RUMO
                                      + WRK-VALOR-JUR-AUX
             END-IF
MJ0001       MOVE WRK-VLJUR-RUMO       TO WRK-VALOR-S13-D2
           ELSE
             IF WRK-I-SITUA(IND-1)     EQUAL 'A VENCER'
               COMPUTE B003-JURPGORI-CAP9 =
                       B003-JURPGORI-CAP9 +
                       WRK-VALOR-JUR-AUX
             END-IF

             MOVE B003-JURPGORI-CAP9   TO WRK-VALOR-S13-D2

           END-IF

           MOVE WRK-VALOR-Z13-D2       TO WRK-I-VLENC-N     (IND-1)

           COMPUTE WRK-SEQ-PCELA       EQUAL ( WRK-SEQ-PCELA + 1 ).

      *----------------------------------------------------------------*
       5100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5110-FORMATAR-SITUACAO          SECTION.
      *----------------------------------------------------------------*
 
           MOVE 'S'                    TO  WRK-CONTR-LIQUIDADO.

           IF (B004-CH-TIPO         OF RUCAB004
                                       NOT LESS  ZEROS )   AND
              (B004-CH-NUMERO       OF RUCAB004
                                       NOT LESS  1     )
               IF (B004-CONTROLE    OF RUCAB004 (1:1)
                                       LESS      WRK-BYTE-LIGADO)
                   MOVE 'N'            TO        WRK-CONTR-LIQUIDADO
               END-IF
           END-IF.

           IF  WRK-CONTR-LIQUIDADO     EQUAL 'S'
               MOVE 'LIQUIDADA'        TO        WRK-I-SITUA(IND-1)
           ELSE
               MOVE B004-DTEVENTO   OF RUCAB004
                                       TO        WRK-DATA-DB2
               MOVE WRK-DD-DB2         TO        WRK-DD-VCT
               MOVE WRK-MM-DB2         TO        WRK-MM-VCT
               MOVE WRK-AAAA-DB2       TO        WRK-AAAA-VCT
               IF   WRK-DATA-VCTO-R    LESS      WRK-DATA-CURR-R
MJ0001            IF VPCELA      OF RUMOV000 EQUAL ZEROS  AND
MJ0001               VPROVS-JURO OF RUMOV000 EQUAL ZEROS
MJ0001              MOVE 'LIQUIDADA'   TO        WRK-I-SITUA(IND-1)
MJ0001            ELSE
                    MOVE 'VENCIDA'     TO        WRK-I-SITUA(IND-1)
MJ0001            END-IF
               ELSE
                    MOVE 'A VENCER'    TO        WRK-I-SITUA(IND-1)
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       5110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

MJ0001*----------------------------------------------------------------*
.      5111-VERIFICAR-RUMO             SECTION.
.     *----------------------------------------------------------------*
.
.          INITIALIZE RUMOV000
.
.          MOVE WRK-I-NROPER-N         TO CCONTR           OF RUMOV000.
.          MOVE B004-DTEVENTO          OF RUCAB004
.                                      TO DVCTO            OF RUMOV000.
.
.          EXEC SQL
.               SELECT VPCELA
.                     ,VPROVS_JURO
.                     ,VJURO_REMUN_ATRSO
.                     ,VJURO_MORA_PCELA
.                     ,VMULTA_CONTR_ATRSO
.                     ,VJURO_CREDT_ATRSO
.                     ,VJURO_MORA_CREDT
.                     ,VMULTA_CREDT_ATRSO
.               INTO  :RUMOV000.VPCELA
.                    ,:RUMOV000.VPROVS-JURO
.                    ,:RUMOV000.VJURO-REMUN-ATRSO
.                    ,:RUMOV000.VJURO-MORA-PCELA
.                    ,:RUMOV000.VMULTA-CONTR-ATRSO
.                    ,:RUMOV000.VJURO-CREDT-ATRSO
.                    ,:RUMOV000.VJURO-MORA-CREDT
.                    ,:RUMOV000.VMULTA-CREDT-ATRSO
.               FROM   DB2PRD.V01PARCELA_PGTO
.               WHERE  CCONTR     =  :RUMOV000.CCONTR
.               AND    DVCTO      =  :RUMOV000.DVCTO
.               WITH   UR
.          END-EXEC.
.
.          IF (SQLCODE             NOT EQUAL ZEROS AND +100)
.          OR (SQLWARN0                EQUAL 'W')
.              MOVE 'DB2'              TO ERR-TIPO-ACESSO
.              MOVE 'RUMOV000'         TO ERR-DBD-TAB
.              MOVE 'SELECT'           TO ERR-FUN-COMANDO
.              MOVE SQLCODE            TO ERR-SQL-CODE
.              MOVE '5111'             TO ERR-LOCAL
.              MOVE SPACES             TO ERR-SEGM
.              PERFORM 9999-ROTINA-ERRO
.          END-IF.
.
.          IF  SQLCODE                 EQUAL +100
.              MOVE ZEROS              TO WRK-VLJUR-RUMO
.          ELSE
.              COMPUTE WRK-VLJUR-RUMO = VJURO-REMUN-ATRSO  OF RUMOV000
.                                     + VJURO-MORA-PCELA   OF RUMOV000
.                                     + VMULTA-CONTR-ATRSO OF RUMOV000
.                                     + VJURO-CREDT-ATRSO  OF RUMOV000
.                                     + VJURO-MORA-CREDT   OF RUMOV000
.                                     + VMULTA-CREDT-ATRSO OF RUMOV000
.          END-IF
.          .
.     *----------------------------------------------------------------*
.      5111-99-FIM.                    EXIT.
MJ0001*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5200-ABRIR-CSR02                SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-I-NROPER-N         TO B004-CH-CONTR    OF RUCAB004.
           MOVE 0                      TO B004-VLPRICOR    OF RUCAB004.
           MOVE '6'                    TO B004-DEBCRED     OF RUCAB004.
           MOVE 23                     TO B004-CODLANC     OF RUCAB004.
           MOVE 'N'                    TO WRK-FIM-CSR.

           EXEC SQL
                OPEN CSR02-RUCAB004
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS )
           OR (SQLWARN0                EQUAL 'W'       )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUCAB004'         TO ERR-DBD-TAB
               MOVE 'OPEN'             TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '5200'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       5200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5300-LER-CSR02                  SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR02-RUCAB004
                 INTO :RUCAB004.B004-CH-NUMERO
                     ,:RUCAB004.B004-DTEVENTO
                           :WRK-DTEVENTO-NULL
                     ,:RUCAB004.B004-VLPRICOR
                     ,:RUCAB004.B004-JURCORRI
                     ,:RUCAB004.B004-CH-TIPO
                     ,:RUCAB004.B004-CONTROLE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W'               )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUCAB004'         TO ERR-DBD-TAB
               MOVE 'FETCH'            TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '5300'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CSR
               GO                      TO 5300-99-FIM
           END-IF.

           IF  WRK-DTEVENTO-NULL       NOT EQUAL ZEROS
               MOVE SPACES             TO  B004-DTEVENTO
                                       OF  RUCAB004
           END-IF.

      *----------------------------------------------------------------*
       5300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5400-FECHAR-CSR02               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR02-RUCAB004
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS )
           OR (SQLWARN0                EQUAL 'W'       )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUCAB004'         TO ERR-DBD-TAB
               MOVE 'CLOSE'            TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '5400'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       5400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5500-TRATAR-PF7                 SECTION.
      *----------------------------------------------------------------*

           IF  WRK-I-PAGINA            EQUAL 001
               MOVE 'NAO HA MAIS PAGINAS PARA VOLTAR'
                                       TO WRK-O-MENSA
               GO TO 5500-99-FIM
           END-IF.

           MOVE WRK-I-PAGINA           TO WRK-PAGINA.
           SUBTRACT 1                  FROM WRK-PAGINA.
           MOVE WRK-PAGINA             TO WRK-I-PAGINA
                                          WRK-O-PAGINA.

           PERFORM 5000-MONTAR-LISTA.

           PERFORM 3200-DEVOLVER-TELA.

           MOVE 'CONSULTA EFETUADA.'   TO WRK-O-MENSA.

      *----------------------------------------------------------------*
       5500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5600-TRATAR-PF8                 SECTION.
      *----------------------------------------------------------------*

           IF  WRK-I-FIM               EQUAL 'S'
               MOVE 'NAO HA MAIS PAGINAS PARA AVANCAR'
                                       TO WRK-O-MENSA
               GO TO 5600-99-FIM
           END-IF.

           MOVE WRK-O-PAGINA           TO WRK-PAGINA.
           ADD  1                      TO WRK-PAGINA.
           MOVE WRK-PAGINA             TO WRK-I-PAGINA
                                          WRK-O-PAGINA.
           PERFORM 5000-MONTAR-LISTA.

           PERFORM 3200-DEVOLVER-TELA.

           MOVE 'CONSULTA EFETUADA.'   TO WRK-O-MENSA.

      *----------------------------------------------------------------*
       5600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5700-TRATAR-PF4                 SECTION.
      *----------------------------------------------------------------*

           PERFORM 5710-IMPRIMIR-DADOS.

           PERFORM 3500-CALCULAR-SALDO-DEVATU.
           MOVE WRK-ACU-SDVATU         TO WRK-VALOR-S13-D2.
           MOVE WRK-VALOR-Z13-D2       TO WRK-I-SDVATU.

           PERFORM 3800-CALCULAR-TOTAL-PCREM.
           MOVE WRK-ACU-TPCREM         TO WRK-NUM-09-CS.
           MOVE WRK-NUM-09-SS          TO WRK-NRO-09.
           MOVE WRK-NRO-03             TO WRK-I-TPCREM.
           IF  WRK-ACU-TPCREM          GREATER ZEROS
MJ0001         COMPUTE WRK-VALOR-JUR-AUX = B003-JURPGAC-PAG8
                                     / WRK-ACU-TPCREM
           ELSE
               MOVE B003-JURPGAC-PAG8  TO WRK-VALOR-JUR-AUX
           END-IF

           PERFORM 5000-MONTAR-LISTA.

           PERFORM 3200-DEVOLVER-TELA.

      *----------------------------------------------------------------*
       5700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5710-IMPRIMIR-DADOS             SECTION.
      *----------------------------------------------------------------*

           MOVE  WRK-I-PAGIMP          TO   WRK-E-PAGINA-IMP
           MOVE  WRK-I-DTBASE          TO   WRK-E-DTBASE-IMP
           MOVE  WRK-I-NROPER          TO   WRK-E-NROPER-IMP
           MOVE  WRK-I-MDOPER          TO   WRK-E-MDOPER-IMP
           MOVE  WRK-I-SDVATU          TO   WRK-E-SDVATU-IMP
           MOVE  WRK-I-TXJANU          TO   WRK-E-TXJANU-IMP
           MOVE  WRK-I-TPCELA          TO   WRK-E-TPCELA-IMP
           MOVE  WRK-I-TXJNOM          TO   WRK-E-TXJNOM-IMP
           MOVE  WRK-I-TPCREM          TO   WRK-E-TPCREM-IMP
           MOVE  WRK-I-TXJEFE          TO   WRK-E-TXJEFE-IMP
           MOVE  WRK-I-SIPGTO          TO   WRK-E-SIPGTO-IMP
           MOVE  WRK-I-DTULVE          TO   WRK-E-DTULVE-IMP
           MOVE  ZEROS                 TO   WRK-S-COD-RETORNO-IMP.

           INITIALIZE WRK-IMPRESSORA.

           CALL 'POOL1080'             USING LNK-IO-PCB
                                             WRK-I-SENHAS
                                             WRK-IMPRESSORA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'IMPRESSORA DESCONECTADA'
                                       TO  WRK-O-MENSA
               GO TO 5710-99-FIM
           END-IF.

           MOVE WRK-IMPRESSORA         TO WRK-E-IMPRESSORA-IMP.
           MOVE WRK-COD-USER-X         TO WRK-E-COD-USER-IMP.
           MOVE 'RUCA6691'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-AREA-RUCA6691
                                             LNK-IO-PCB
                                             LNK-ALT-PCB

           EVALUATE WRK-S-COD-RETORNO-IMP
               WHEN ZEROS
                    MOVE 'IMPRESSAO CONCLUIDA'
                                       TO   WRK-O-MENSA
                    MOVE ZEROS         TO   WRK-I-PAGIMP
               WHEN 1
                    MOVE 'IMPRESSORA DESCONECTADA'
                                       TO   WRK-O-MENSA
               WHEN 3
                    MOVE 'NAO HA DADOS PARA IMPRESSAO'
                                       TO   WRK-O-MENSA
               WHEN 4
                    MOVE 'TECLE <PF4> PARA CONTINUAR A IMPRESSAO'
                                       TO   WRK-O-MENSA
                    MOVE WRK-E-PAGINA-IMP
                                       TO   WRK-I-PAGIMP
               WHEN OTHER
                    MOVE 'ERRO NA IMPRESSAO'
                                       TO   WRK-O-MENSA
           END-EVALUATE.

      *----------------------------------------------------------------*
       5710-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       7000-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF WRK-OUTPUT   TO WRK-660-LL-OUTPUT
                                          WRK-O-LL
           MOVE LENGTH OF WRK-660-RUCAT690
                                       TO WRK-660-LL

           CALL  'BRAD0660'            USING WRK-OUTPUT
                                             WRK-660-RUCAT690.

           IF RETURN-CODE              NOT EQUAL  ZEROS
              MOVE  'APL'              TO ERR-TIPO-ACESSO
              MOVE  RETURN-CODE        TO WRK-RETURN-CODE
              MOVE  70                 TO WRK-LOCAL-ERRO
              MOVE  WRK-ERRO-BRAD0660  TO ERR-TEXTO
              PERFORM  9999-ROTINA-ERRO
           END-IF.

           MOVE  WRK-ISRT              TO WRK-FUNCAO.
           MOVE  WRK-OUTPUT            TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       7000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       8000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       8000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           IF ERR-MODULO               NOT EQUAL  SPACES  AND
                                                  LOW-VALUES
              MOVE  'RUCA6690'         TO  ERR-PGM
           ELSE
              MOVE  'SENHAS02'         TO  ERR-PGM
              MOVE  'RUCA6690'         TO  ERR-MODULO
           END-IF.

           MOVE  WRK-COD-USER-X        TO  ERR-COD-USER.
           MOVE  WRK-COD-DEPTO         TO  ERR-COD-DEPTO.


           IF  ERR-TIPO-ACESSO         EQUAL 'DB2'
               CALL  'BRAD7100'        USING  LNK-IO-PCB
                                              ERRO-AREA
                                              LNK-ALT-PCB
                                              SQLCA
           ELSE
               CALL 'BRAD7100'         USING  LNK-IO-PCB
                                              ERRO-AREA
                                              LNK-ALT-PCB
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
