       IDENTIFICATION DIVISION.
       PROGRAM-ID. GFCT1992.
       AUTHOR. NARDAO.
      *----------------------------------------------------------------*
      *                 CPM  CONSULTORIA                               *
      *----------------------------------------------------------------*
      *                                                                *
      * PROGRAMA     -  GFCT1992                                       *
      * ANALISTA     -  NARDAO - CPM BRAXIS S/A                        *
      * OBJETIVO     -  INCLUSAO DE TARIFA DE ISENCAO                  *
      *                                                                *
      *----------------------------------------------------------------*
      *                                                                *
      * ENTRADA : CADCLIE  - CADASTRO DE CLIENTES           - I#SCCEI2 *
      *         : GFCTB054 - FLEXIBILIZACOES                - LRECL=44 *
      * SAIDA   : CADTARIF - CADASTRO DE TARIFAS            - I#SCCEI1 *
      *         : RELSAI   - RELATORIO DE SAIDA                        *
      *                                                                *
      * CHAMA   : POOL7600 - DATA/HORA DO SISTEMA                      *
      *         : ILBOABN0 - FORCA O ABEND (+1111)                     *
      *                                                                *
      *----------------------------------------------------------------*

HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA  * ***  HEXASOLUTION - 08/2009 - CONVERSAO FAIXA DE AGENCIAS  *** *
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA  *                       TEMPLATE_OCCURS                          *
HEXA  *----------------------------------------------------------------*
HEXA  *  TABELA: TABELA-TAXA                                           *
HEXA  *  AREA DE EXPANSAO: NAO                                         *
HEXA  *  INDEXADOR: NAO-EXISTE                                         *
HEXA  *  SUBSCRITOR: IND/IND2                                          *
HEXA  *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CADCLIE ASSIGN TO UT-S-CADCLIE
                     FILE STATUS IS WRK-FS-CADCLIE.

           SELECT GFCTB054 ASSIGN TO UT-S-GFCTB054
                      FILE STATUS IS WRK-FS-GFCTB054.

           SELECT CADTARIF ASSIGN TO UT-S-CADTARIF
                      FILE STATUS IS WRK-FS-CADTARIF.

           SELECT RELSAI ASSIGN TO UT-S-RELSAI
                    FILE STATUS IS WRK-FS-RELSAI.

           SELECT RELSAI2 ASSIGN TO UT-S-RELSAI2
                    FILE STATUS IS WRK-FS-RELSAI2.
       DATA DIVISION.
       FILE SECTION.

       FD  CADCLIE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.


      ******************************************************************
      *      CADASTRO DE CLIENTES - BASE = CLIE                        *
      *                                                                *
      *      CLASSIF. AG/POSTO/CONTA/RAZAO                             *
      *                                                                *
      *    INC = I#SCCEI2         LRECL = 0065          RECFM = FB     *
      ******************************************************************

       01  REG-CADACLIE.
           03  CAD-CHAVE.
               05  CAD-AGENCIA          PIC  9(05)     COMP-3.
               05  CAD-RAZAO            PIC  9(05)     COMP-3.
               05  CAD-CONTA            PIC  9(07)     COMP-3.
           03  CAD-DIG-CTA              PIC  X(01).
           03  CAD-CGC-CPF.
               05  CAD-PRINCIPAL        PIC  9(09)     COMP-3.
               05  CAD-FILIAL           PIC  9(05)     COMP-3.
               05  CAD-CTLE             PIC  9(02).
           03  CAD-NOME-CLIE            PIC  X(40).
           03  CAD-POSTO-SERV           PIC  9(03)     COMP-3.
           03  CAD-IDENTF-CTA           PIC  X(02).
ST25X6     03  CAD-CGC-CPF-ST.
ST25X6         05  CAD-PRINCIPAL-ST     PIC  X(09).
ST25X6         05  CAD-FILIAL-ST        PIC  X(04).
ST25X6         05  CAD-CTLE-ST          PIC  9(02).

       FD  GFCTB054
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-GFCTB054.
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
           10 CAGPTO-CTA           PIC S9(3)V USAGE COMP-3.
           10 HIDTFD-FLEXZ-AGPTO   PIC X(26).
           10 CEMPR-INC            PIC S9(5)V USAGE COMP-3.
           10 CDEPDC               PIC S9(5)V USAGE COMP-3.
           10 CPOSTO-SERVC         PIC S9(5)V USAGE COMP-3.
           10 PDESC-FLEXZ-AGPTO    PIC S9(3)V9(4) USAGE COMP-3.

       FD  CADTARIF
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

      ** I#SCCEIS *****************************************************
      *                                                               *
      *  CADASTRO DE CONTAS COM ISENCAO DE TARIFA.                    *
      *                                         LRECL=030             *
      *****************************************************************
           SKIP2
       01  REGSCCE.
           03  SCCE-CHAVE.
               05  SCCE-AGENCIA         PIC 9(005)    COMP-3.
               05  SCCE-GRUPO-RZ        PIC 9(003)    COMP-3.
               05  SCCE-SUBGP-RZ        PIC 9(003)    COMP-3.
               05  SCCE-CONTA           PIC 9(007)    COMP-3.
               05  SCCE-TAR-ISEN        PIC 9(005)    COMP-3.
           03  SCCE-PCENT-ISEN          PIC 9(003)V99 COMP-3.
           03  SCCE-PER-COBR            PIC X(001).
           03  SCCE-DEMONST-LANCTO      PIC X(001).
           03  SCCE-ISENTA-COBRA        PIC X(001).
           03  SCCE-PAB-PARM            PIC 9(003)    COMP-3.
           03  FILLER                   PIC X(004).
           03  SCCE-ORIG-ISENCAO        PIC X(004).

       FD  RELSAI
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-RELSAI.
           03  REG-LINHA              PIC X(133).

       FD  RELSAI2
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-RELSAI2.
           03  REG-LINHA2             PIC X(133).

       WORKING-STORAGE SECTION.

       77  FILLER                     PIC X(30) VALUE
           'INICIO DA W.S.S. PGM GFCT1992'.

       77  WRK-OPERACAO               PIC X(013) VALUE SPACES.
       77  WRK-ABERTURA               PIC X(013) VALUE ' NA ABERTURA '.
       77  WRK-LEITURA                PIC X(013) VALUE ' NA  LEITURA '.
       77  WRK-GRAVACAO               PIC X(013) VALUE ' NA GRAVACAO '.
       77  WRK-FECHAMENTO             PIC X(013) VALUE 'NO FECHAMENTO'.
       77  WRK-ABEND                  PIC S9(04) COMP VALUE +1111.
       77  WRK-FS-CADCLIE             PIC X(002) VALUE SPACES.
       77  WRK-FS-CADTARIF            PIC X(002) VALUE SPACES.
       77  WRK-FS-GFCTB054            PIC X(002) VALUE SPACES.
       77  WRK-FS-RELSAI              PIC X(002) VALUE SPACES.
       77  WRK-FS-RELSAI2             PIC X(002) VALUE SPACES.
       77  XAVE                       PIC 9(001) VALUE ZEROS.
       77  IND                        PIC 9(004) VALUE ZEROS.
       77  IND2                       PIC 9(004) VALUE ZEROS.

       01  WS-LIDOS-CADCLIE           PIC 9(007) VALUE ZEROS.
       01  WS-LIDOS-GFCTB054          PIC 9(007) VALUE ZEROS.
       01  WS-GRAVA-CADTARIF          PIC 9(007) VALUE ZEROS.
       01  WS-DISPLAY-EDITADO         PIC Z.ZZZ.ZZ9 VALUE ZEROS.

       01  ACUMULADOR-AGENCIA         PIC 9(015) VALUE ZEROS.
       01  ACUMULADOR-PAB             PIC 9(015) VALUE ZEROS.
       01  TOTAL-AGENCIA              PIC 9(015) VALUE ZEROS.
       01  TOTAL-PABS                 PIC 9(015) VALUE ZEROS.


       01  CHAVE-CADCLIE.
           03  CHV-AGENCIA-CADCLIE    PIC 9(005).
           03  CHV-PAB-CADCLIE        PIC 9(005).

       01  CHAVE-GFCTB054.
           03  CHV-AGENCIA-GFCTB054   PIC 9(005).
           03  CHV-PAB-GFCTB054       PIC 9(005).

       01  CHAVE-GFCTB054-ANT.
           03  CHV-AGENCIA-GFCTB054-ANT PIC 9(005).
           03  CHV-PAB-GFCTB054-ANT    PIC 9(005).

       01  TABZERA.
           03  FILLER                  PIC 9(005)       VALUE ZEROS.
           03  FILLER                  PIC 9(005)       VALUE ZEROS.
           03  FILLER                  PIC 9(005)       VALUE ZEROS.
           03  FILLER                  PIC 9(003)V9(04) VALUE ZEROS.
           03  FILLER                  PIC 9(015)       VALUE ZEROS.
           03  TABELA-TAXA.
HEXA  **********   INICIO      TEMPLATE_OCCURS
HEXA  ***  ALTERADO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA
HEXA  *
HEXA  *        05  TABELA-TAXA-OCCURS OCCURS 5000 TIMES.
HEXA  *
HEXA           05  TABELA-TAXA-OCCURS OCCURS 9999 TIMES.
HEXA  **********   FIM         TEMPLATE_OCCURS
                   07  AGEN-TAB-TAXA       PIC 9(005).
                   07  PAB-TAB-TAXA        PIC 9(005).
                   07  TAXA-TAB-TAXA       PIC 9(005).
                   07  PERC-TAB-TAXA       PIC 9(003)V9(04).
                   07  QTDE-TAB-TAXA       PIC 9(015).

       01  WRK-DATA-HORA.
           03  DT-JULIANA             PIC 9(005) COMP-3.
           03  DT-AAMMDD              PIC 9(007) COMP-3.
           03  DT-AAAAMMDD            PIC 9(009) COMP-3.
           03  DT-HHMMSS              PIC 9(007) COMP-3.
           03  DT-HHMMSSMMMMMM        PIC 9(013) COMP-3.
           03  TIMESTAMP.
               05  TS-DATA            PIC 9(008).
               05  FILLER REDEFINES TS-DATA.
                   07  TS-ANO         PIC 9(004).
                   07  TS-MES         PIC 9(002).
                   07  TS-DIA         PIC 9(002).
               05  TS-HORA            PIC X(006).
               05  FILLER REDEFINES TS-HORA.
                   07  TS-HORA-N      PIC 9(006).
                   07  FILLER REDEFINES TS-HORA-N.
                       09  TS-HH      PIC 9(002).
                       09  TS-MM      PIC 9(002).
                       09  TS-SS      PIC 9(002).
               05  TS-ZEROS           PIC 9(006).


       01  WRK-DATA                   PIC 9(008) VALUE ZEROS.
       01  FILLER REDEFINES WRK-DATA.
           03  WRK-DIA                PIC 9(002).
           03  WRK-MES                PIC 9(002).
           03  WRK-ANO                PIC 9(004).

       01  WRK-CTLIN                  PIC 9(002) VALUE 90.
       01  WRK-CTLIN2                 PIC 9(002) VALUE 99.
       01  WRK-CTA-PAG                PIC 9(004) VALUE ZEROS.
       01  WRK-CTA-PAG2               PIC 9(004) VALUE ZEROS.
       01  WRK-AGENCIA-ANTERIOR       PIC 9(005) VALUE ZEROS.
       01  WRK-PAB-ANTERIOR           PIC 9(005) VALUE ZEROS.

      *---------------------------------------------------------------*
      *                            CABECALHOS                         *
      *---------------------------------------------------------------*
       01  CABEC01.
           03  FILLER                  PIC X(02)  VALUE '1'.
           03  FILLER                  PIC X(17)  VALUE
           'B R A D E S C O'.
           03  FILLER                  PIC X(55)  VALUE
           'C/C CADASTRAMENTO DE CONTAS E EVENTOS POR AGENCIA/PAB'.
           03  CB01-HORA.
               05  CB01-HH             PIC 9(02).
               05  FILLER              PIC X(01)  VALUE '.'.
               05  CB01-MM             PIC 9(02).
               05  FILLER              PIC X(01)  VALUE '.'.
               05  CB01-SS             PIC 9(02).
           03  FILLER                  PIC X(01)  VALUE SPACES.
           03  CABEC01-PAGINA          PIC Z.ZZZ.ZZ9.

       01  CABEC01-A.
           03  FILLER                  PIC X(02)  VALUE '1'.
           03  FILLER                  PIC X(17)  VALUE
           'B R A D E S C O'.
           03  FILLER                  PIC X(55)  VALUE
           'C/C CADASTRAMENTO DE CONTAS E EVENTOS POR AGENCIA/PAB'.
           03  CB01-HORAA.
               05  CB01-HHA            PIC 9(02).
               05  FILLER              PIC X(01)  VALUE '.'.
               05  CB01-MMA            PIC 9(02).
               05  FILLER              PIC X(01)  VALUE '.'.
               05  CB01-SSA            PIC 9(02).
           03  FILLER                  PIC X(01)  VALUE SPACES.
           03  CABEC01-PAGINAA         PIC Z.ZZZ.ZZ9.

       01  CABEC02.
           03  FILLER                  PIC X(02)  VALUE '0'.
           03  FILLER                  PIC X(65)  VALUE
               'GFCT1992  RELATORIO DE INCLUSAO DE ISENCAO DE TARIFAS'.
           03  CB02-DATA.
               05  CB02-DIA            PIC X(02).
               05  FILLER              PIC X(01)  VALUE '/'.
               05  CB02-MES            PIC X(02).
               05  FILLER              PIC X(01)  VALUE '/'.
               05  CB02-ANO            PIC X(04).

       01  CABEC03.
           03  FILLER                  PIC X(02)  VALUE '0'.
           03  FILLER                  PIC X(13)  VALUE
               'AGENCIA   -  '.
           03  CABEC03-AGENCIA         PIC ZZZZ9.

       01  CABEC04.
           03  FILLER                  PIC X(02)  VALUE '0'.
           03  FILLER                  PIC X(23)  VALUE
               'POSTO DE ATENDIMENTO - '.
           03  CABEC04-POSTO           PIC ZZZZ9.

       01  CABEC05.
           03  FILLER                  PIC X(01)  VALUE '0'.
           03  FILLER                  PIC X(67)  VALUE
               'CONTA        RAZAO  NOME'.
           03  FILLER                  PIC X(20)  VALUE
               'TARIFA  PERC.ISEN'.

       01  CABEC03A.
           03  FILLER                  PIC X(01)  VALUE '0'.
           03  FILLER                  PIC X(45)  VALUE
               'AGENCIA  PAB   TAXA  PERCENT.  QTDE CONTAS'.

       01  DETALHE.
           03  CARRO-DETALHE           PIC X(01)  VALUE '0'.
           03  LIN-CONTA               PIC 9(07)  VALUE ZEROS.
           03  FILLER                  PIC X(01)  VALUE '-'.
           03  LIN-DIG                 PIC X(01)  VALUE SPACES.
           03  FILLER                  PIC X(04)  VALUE SPACES.
           03  LIN-RAZAO               PIC 9(05)  VALUE ZEROS.
           03  FILLER                  PIC X(02)  VALUE SPACES.
           03  LIN-NOME                PIC X(40)  VALUE SPACES.
           03  FILLER                  PIC X(07)  VALUE SPACES.
           03  LIN-TARIFA              PIC 9(07)  VALUE ZEROS.
           03  FILLER                  PIC X(03)  VALUE SPACES.
           03  LIN-PERCENT             PIC ZZ9,99 VALUE ZEROS.

       01  DETALHE2.
           03  CARRO-DETALHE2          PIC X(01)  VALUE '0'.
           03  LIN-AGENCIA             PIC 9(05)  VALUE ZEROS.
           03  FILLER                  PIC X(04)  VALUE SPACES.
           03  LIN-PAB2                PIC 9(05)  VALUE ZEROS.
           03  FILLER                  PIC X(01)  VALUE SPACES.
           03  LIN-TAXA2               PIC 9(05)  VALUE ZEROS.
           03  FILLER                  PIC X(02)  VALUE SPACES.
           03  LIN-PERCENT2            PIC ZZZ,99   VALUE ZEROS.
           03  FILLER                  PIC X(04)  VALUE SPACES.
           03  LIN-QTDE                PIC 9(07)  VALUE ZEROS.

       01  TOTAL1.
           03  FILLER                  PIC X(01)  VALUE '0'.
           03  FILLER                  PIC X(18)  VALUE
           'TOTAL DA AGENCIA  '.
           03  TOTAL1-CODAGEN          PIC ZZZZ9  VALUE ZEROS.
           03  FILLER                  PIC X(03)  VALUE ' - '.
           03  TOTAL1-AGENCIA          PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  TOTAL2.
           03  FILLER                  PIC X(01)  VALUE '0'.
           03  FILLER                  PIC X(13)  VALUE
           'TOTAL DO PAB '.
           03  TOTAL2-CODPAB           PIC ZZZZ9  VALUE ZEROS.
           03  FILLER                  PIC X(03)  VALUE ' - '.
           03  TOTAL2-PAB              PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  TOTAL-GERAL-AG.
           03  FILLER                  PIC X(01)  VALUE '0'.
           03  FILLER                  PIC X(20)  VALUE
           'TOTAL DE AGENCIAS : '.
           03  TOTAL-AGENCIA-T         PIC ZZZZZZZZ9  VALUE ZEROS.

       01  TOTAL-GERAL-PAB.
           03  FILLER                  PIC X(01)  VALUE '0'.
           03  FILLER                  PIC X(20)  VALUE
           'TOTAL DE PABS     : '.
           03  TOTAL-PAB-T             PIC ZZZZZZZZ9  VALUE ZEROS.

       01  RAZAO-WORK.
           03  GRUPO-RAZAO            PIC 9(2).
           03  SUBGRP-RAZAO           PIC 9(2).
           03  RESTO-RAZAO            PIC 9(1).
       01  WK-RAZAO REDEFINES RAZAO-WORK PIC 9(5).

HEXA  *
HEXA  *---------------------------------------------------------------*
HEXA  * AREA DE DECLARACAO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA *
HEXA  *---------------------------------------------------------------*
HEXA  *
HEXA       COPY I#HEXA01.
HEXA  *
       77  FILLER                     PIC X(30) VALUE
           'FIM DA W.S.S. PGM GFCT1992'.

       PROCEDURE DIVISION.

       0001-INICIO.
HEXA  *
HEXA  *---------------------------------------------------------------*
HEXA  * AREA DE MOVIMENTACAO DA ROTINA MESU E DE ABEND                *
HEXA  *---------------------------------------------------------------*
HEXA  *
HEXA       MOVE        WRK-ULTIMA-AG-U TO      WRK-HX01-U-5.
HEXA  *

           OPEN INPUT  CADCLIE,
                       GFCTB054,
                OUTPUT CADTARIF,
                       RELSAI
                       RELSAI2.

           CALL 'POOL7600' USING WRK-DATA-HORA
           MOVE TS-DIA TO CB02-DIA.
           MOVE TS-MES TO CB02-MES.
           MOVE TS-ANO TO CB02-ANO.
           MOVE TS-HH  TO CB01-HH.
           MOVE TS-MM  TO CB01-MM.
           MOVE TS-SS  TO CB01-SS.
           MOVE CB01-HORA TO CB01-HORAA.

           MOVE WRK-ABERTURA TO WRK-OPERACAO

           PERFORM 1000-TESTAR-FILE-STATUS.

           PERFORM 2000-LER-CADCLIE.

           PERFORM 2010-LER-GFCTB054.

           PERFORM 2100-TESTAR-SE-CADCLIE-VAZIO.

           PERFORM 2110-TESTAR-SE-GFCTB054-VAZIO.

           PERFORM 3000-PROCESSAMENTO UNTIL WRK-FS-GFCTB054 EQUAL '10'.

           PERFORM 9000-PROCEDIMENTOS-FINAIS.

       0001-99-EXIT. EXIT.


       1000-TESTAR-FILE-STATUS SECTION.

           PERFORM 1001-TESTE-CADCLIE.
           PERFORM 1002-TESTE-GFCTB054.
           PERFORM 1003-TESTE-CADTARIF.
           PERFORM 1004-TESTE-RELSAI.
           PERFORM 1005-TESTE-RELSAI2.

       1000-TESTAR-FILE-STATUS-FIM.  EXIT.

       1001-TESTE-CADCLIE SECTION.

           IF  WRK-FS-CADCLIE NOT EQUAL '00'
               DISPLAY ' '
               DISPLAY ' ************* GFCT1992 ******************'
               DISPLAY
                       ' *   ERRO ' WRK-OPERACAO ' DO ARQUIVO       *'
               DISPLAY ' *              CADCLIE                  *'
               DISPLAY ' *        FILE STATUS =  ' WRK-FS-CADCLIE
                                                     '              *'
               DISPLAY ' ************* GFCT1992 ******************'
               DISPLAY '                                           '
               CALL 'ILBOABN0' USING WRK-ABEND
           END-IF.

       1001-TESTE-CXUNICO-FIM. EXIT.

       1002-TESTE-GFCTB054  SECTION.

           IF  WRK-FS-GFCTB054  NOT EQUAL '00'
               DISPLAY ' '
               DISPLAY ' ************** GFCT1992 ******************'
               DISPLAY
                       ' *   ERRO ' WRK-OPERACAO ' DO ARQUIVO        *'
               DISPLAY ' *             GFCTB054                    *'
               DISPLAY ' *        FILE STATUS =  ' WRK-FS-GFCTB054
                                                     '               *'
               DISPLAY ' ************** GFCT1992 ******************'
               DISPLAY '                                           '
               CALL 'ILBOABN0' USING WRK-ABEND
           END-IF.

       1002-TESTE-GFCTB054-FIM.  EXIT.

       1003-TESTE-CADTARIF SECTION.

           IF  WRK-FS-CADTARIF NOT EQUAL '00'
               DISPLAY ' '
               DISPLAY ' ************** GFCT1992 ******************'
               DISPLAY
                       ' *   ERRO ' WRK-OPERACAO ' DO ARQUIVO        *'
               DISPLAY ' *              CADTARIF                  *'
               DISPLAY ' *        FILE STATUS =  ' WRK-FS-CADTARIF
                                                     '               *'
               DISPLAY ' ************** GFCT1992 ******************'
               DISPLAY '                                           '
               CALL 'ILBOABN0' USING WRK-ABEND
           END-IF.

       1003-TESTE-CADTARIF-FIM.  EXIT.

       1004-TESTE-RELSAI SECTION.

           IF  WRK-FS-RELSAI NOT EQUAL '00'
               DISPLAY ' '
               DISPLAY ' ************** GFCT1992 ******************'
               DISPLAY
                       ' *   ERRO ' WRK-OPERACAO ' DO ARQUIVO        *'
               DISPLAY ' *              RELSAI                    *'
               DISPLAY ' *        FILE STATUS =  ' WRK-FS-RELSAI
                                                     '               *'
               DISPLAY ' ************** GFCT1992 ******************'
               DISPLAY '                                           '
               CALL 'ILBOABN0' USING WRK-ABEND
           END-IF.

       1004-TESTE-RELSAI-FIM.  EXIT.

       1005-TESTE-RELSAI2 SECTION.

           IF  WRK-FS-RELSAI2 NOT EQUAL '00'
               DISPLAY ' '
               DISPLAY ' ************** GFCT1992 ******************'
               DISPLAY
                       ' *   ERRO ' WRK-OPERACAO ' DO ARQUIVO        *'
               DISPLAY ' *              RELSAI2                   *'
               DISPLAY ' *        FILE STATUS =  ' WRK-FS-RELSAI2
                                                     '               *'
               DISPLAY ' ************** GFCT1992 ******************'
               DISPLAY '                                           '
               CALL 'ILBOABN0' USING WRK-ABEND
           END-IF.

       1005-TESTE-RELSAI2-FIM.  EXIT.

       2000-LER-CADCLIE SECTION.

           READ CADCLIE.

           IF  WRK-FS-CADCLIE EQUAL '10'
               MOVE  HIGH-VALUES TO CHAVE-CADCLIE
               GO TO 2000-LER-CADCLIE-FIM
           END-IF

           MOVE WRK-LEITURA TO WRK-OPERACAO.
           PERFORM 1001-TESTE-CADCLIE.

           ADD 1 TO WS-LIDOS-CADCLIE.

           MOVE  CAD-AGENCIA  TO  CHV-AGENCIA-CADCLIE.
           MOVE  CAD-POSTO-SERV TO CHV-PAB-CADCLIE.


       2000-LER-CADCLIE-FIM. EXIT.

       2010-LER-GFCTB054  SECTION.

           READ GFCTB054.

           IF  WRK-FS-GFCTB054  EQUAL '10'
               MOVE HIGH-VALUES TO CHAVE-GFCTB054
               GO TO 2010-LER-GFCTB054-FIM.

           MOVE WRK-LEITURA TO WRK-OPERACAO
           PERFORM 1002-TESTE-GFCTB054.

           MOVE  CDEPDC       OF GFCTB054  TO  CHV-AGENCIA-GFCTB054.
           MOVE  CPOSTO-SERVC OF GFCTB054  TO  CHV-PAB-GFCTB054.

           ADD 1 TO WS-LIDOS-GFCTB054.


       2010-LER-GFCTB054-FIM. EXIT.

       2100-TESTAR-SE-CADCLIE-VAZIO SECTION.

           IF  WRK-FS-CADCLIE EQUAL '10'
               DISPLAY ' '
               DISPLAY '*************** GFCT1992 **************'
               DISPLAY '*                                     *'
               DISPLAY '* O PROGRAMA NAO SERA ENCERRADO       *'
               DISPLAY '*                                     *'
               DISPLAY '* CADASTRO CLIENTES ENCONTRA-SE VAZIO *'
               DISPLAY '*                                     *'
               DISPLAY '*************** GFCT1992 **************'
               DISPLAY ' '
           END-IF.

       2100-99-FIM. EXIT.

       2110-TESTAR-SE-GFCTB054-VAZIO SECTION.

           IF  WRK-FS-GFCTB054  EQUAL '10'
               DISPLAY ' '
               DISPLAY '************** GFCT1992 **************'
               DISPLAY '*                                    *'
               DISPLAY '* ARQUIVO GFCTB054  ENCONTRA-SE VAZIO *'
               DISPLAY '*                                    *'
               DISPLAY '* O PROGRAMA NAO SERA ENCERRADO      *'
               DISPLAY '*                                    *'
               DISPLAY '************** GFCT1992 **************'
               DISPLAY ' '
           END-IF.

       2110-99-FIM. EXIT.

       3000-PROCESSAMENTO SECTION.

           IF  CHAVE-GFCTB054  =  CHAVE-CADCLIE
               MOVE  TABZERA  TO TABELA-TAXA
               MOVE CHAVE-GFCTB054  TO CHAVE-GFCTB054-ANT
               MOVE  1        TO IND
HEXA  *----------------------------------------------------------------*
HEXA  *            INICIO     DO     TEMPLATE_PERFORM -   BLOCO  1
HEXA  *----------------------------------------------------------------*
HEXA  ***  ALTERADO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA
HEXA  *
HEXA  *     HX_PERFORM 3050-CARREGA-TABELA UNTIL
HEXA  *                IND GREATER 5000 OR
HEXA  *                CHAVE-GFCTB054 NOT = CHAVE-GFCTB054-ANT
HEXA  *
HEXA           PERFORM 3050-CARREGA-TABELA UNTIL
HEXA                   IND GREATER WRK-HX01-U-4 OR
HEXA                   CHAVE-GFCTB054 NOT = CHAVE-GFCTB054-ANT
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA  *            TERMINO    DO     TEMPLATE_PERFORM -   BLOCO  1
HEXA  *----------------------------------------------------------------*
HEXA  *
               PERFORM 3100-GRAVA-CADTARIF UNTIL CHAVE-GFCTB054-ANT
                                  NOT = CHAVE-CADCLIE
               MOVE  1        TO IND
HEXA  *----------------------------------------------------------------*
HEXA  *            INICIO     DO     TEMPLATE_PERFORM -   BLOCO  1
HEXA  *----------------------------------------------------------------*
HEXA  ***  ALTERADO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA
HEXA  *
HEXA  *     HX_PERFORM 5000-IMPRIME-RELSAI2 UNTIL IND GREATER 5000
HEXA  *                OR AGEN-TAB-TAXA (IND) = ZEROS
HEXA  *
HEXA           PERFORM 5000-IMPRIME-RELSAI2 UNTIL
HEXA                   IND GREATER WRK-HX01-U-4
HEXA                   OR AGEN-TAB-TAXA (IND) = ZEROS
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA  *            TERMINO    DO     TEMPLATE_PERFORM -   BLOCO  1
HEXA  *----------------------------------------------------------------*
HEXA  *
           ELSE
               IF  CHAVE-GFCTB054  GREATER CHAVE-CADCLIE
                   PERFORM 2000-LER-CADCLIE
               ELSE
                   PERFORM 2010-LER-GFCTB054.

       3000-PROCESSAMENTO-FIM. EXIT.

       3050-CARREGA-TABELA SECTION.

           MOVE CDEPDC            OF GFCTB054 TO AGEN-TAB-TAXA (IND).
           MOVE CPOSTO-SERVC      OF GFCTB054 TO PAB-TAB-TAXA (IND).
           MOVE CSERVC-TARIF      OF GFCTB054 TO TAXA-TAB-TAXA (IND).
           MOVE PDESC-FLEXZ-AGPTO OF GFCTB054 TO PERC-TAB-TAXA (IND).
           ADD  1                             TO IND.
           PERFORM 2010-LER-GFCTB054.

       3050-PROCESS-99-FIM. EXIT.

       3100-GRAVA-CADTARIF SECTION.

            MOVE     1     TO  IND2.

HEXA  *----------------------------------------------------------------*
HEXA  *            INICIO     DO     TEMPLATE_PERFORM -   BLOCO  1
HEXA  *----------------------------------------------------------------*
HEXA  ***  ALTERADO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA
HEXA  *
HEXA  *  HX_PERFORM  3110-GRAVA-CLIENTES-TAB UNTIL
HEXA  *              IND2 GREATER 5000 OR
HEXA  *              AGEN-TAB-TAXA (IND2) = ZERO.
HEXA  *
HEXA        PERFORM  3110-GRAVA-CLIENTES-TAB UNTIL
HEXA                 IND2 GREATER WRK-HX01-U-4 OR
HEXA                 AGEN-TAB-TAXA (IND2) = ZERO.
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA  *            TERMINO    DO     TEMPLATE_PERFORM -   BLOCO  1
HEXA  *----------------------------------------------------------------*
HEXA  *

            PERFORM  2000-LER-CADCLIE.

       3100-GRAVA-CADTARIF-FIM. EXIT.

       3110-GRAVA-CLIENTES-TAB SECTION.

           INITIALIZE REGSCCE.

           MOVE  CAD-AGENCIA   TO  SCCE-AGENCIA OF CADTARIF.
           MOVE  CAD-RAZAO     TO  WK-RAZAO LIN-RAZAO.
           MOVE  GRUPO-RAZAO   TO  SCCE-GRUPO-RZ.
           MOVE  SUBGRP-RAZAO TO  SCCE-SUBGP-RZ.
           MOVE  CAD-CONTA     TO  SCCE-CONTA   OF CADTARIF
                                   LIN-CONTA.
           MOVE  CAD-DIG-CTA   TO  LIN-DIG.
           MOVE  CAD-NOME-CLIE TO  LIN-NOME.
           MOVE  TAXA-TAB-TAXA (IND2) TO  SCCE-TAR-ISEN
                                   LIN-TARIFA.
           MOVE  PERC-TAB-TAXA (IND2) TO  SCCE-PCENT-ISEN
                                   LIN-PERCENT.
           ADD   1             TO  QTDE-TAB-TAXA (IND2).
           MOVE  'D'           TO  SCCE-PER-COBR.
           MOVE  'D'           TO  SCCE-DEMONST-LANCTO.
           MOVE  'I'           TO  SCCE-ISENTA-COBRA.
           MOVE CAD-POSTO-SERV TO  SCCE-PAB-PARM.
           MOVE  'GFCT'        TO  SCCE-ORIG-ISENCAO.

           MOVE WRK-GRAVACAO TO WRK-OPERACAO.

           WRITE REGSCCE.

           PERFORM 1003-TESTE-CADTARIF.

           ADD 1 TO WS-GRAVA-CADTARIF.

           PERFORM 3200-IMPRIME-RELATORIO.

           ADD 1 TO IND2.

       3110-GRAVA-CLIENTES-FIM. EXIT.

       3200-IMPRIME-RELATORIO   SECTION.

           IF  XAVE  =  ZERO
               MOVE CAD-AGENCIA TO WRK-AGENCIA-ANTERIOR
                                   CABEC03-AGENCIA
               MOVE CAD-POSTO-SERV TO WRK-PAB-ANTERIOR
                                   CABEC04-POSTO
               ADD  1 TO TOTAL-AGENCIA TOTAL-PABS
               MOVE 1 TO XAVE.

           IF  CAD-AGENCIA  NOT  =  WRK-AGENCIA-ANTERIOR
               MOVE WRK-AGENCIA-ANTERIOR TO CABEC03-AGENCIA
                                            TOTAL1-CODAGEN
               MOVE ACUMULADOR-AGENCIA   TO TOTAL1-AGENCIA
               MOVE WRK-PAB-ANTERIOR     TO CABEC04-POSTO
                                            TOTAL2-CODPAB
               MOVE ACUMULADOR-PAB       TO TOTAL2-PAB
               ADD  1 TO TOTAL-AGENCIA TOTAL-PABS
               PERFORM 4100-IMPRIME-TOTAL-PAB
               PERFORM 4110-IMPRIME-TOTAL-AGENCIA
               MOVE CAD-AGENCIA TO WRK-AGENCIA-ANTERIOR
                                   CABEC03-AGENCIA
               MOVE CAD-POSTO-SERV TO WRK-PAB-ANTERIOR
                                   CABEC04-POSTO
               MOVE 99  TO  WRK-CTLIN
               MOVE '0' TO CARRO-DETALHE
               MOVE 1          TO  ACUMULADOR-AGENCIA
                                   ACUMULADOR-PAB
           ELSE
               IF  CAD-POSTO-SERV NOT = WRK-PAB-ANTERIOR
                    MOVE WRK-PAB-ANTERIOR     TO CABEC04-POSTO
                                              TOTAL2-CODPAB
                    MOVE ACUMULADOR-PAB       TO TOTAL2-PAB
                    MOVE CAD-POSTO-SERV TO WRK-PAB-ANTERIOR
                                              CABEC04-POSTO
                    PERFORM 4100-IMPRIME-TOTAL-PAB
                    MOVE 1     TO ACUMULADOR-PAB
                    ADD  1     TO ACUMULADOR-AGENCIA TOTAL-PABS
                    PERFORM 4120-IMPRIME-CABEC-PAB
                    MOVE '0' TO CARRO-DETALHE
               ELSE
                    ADD  1 TO ACUMULADOR-AGENCIA ACUMULADOR-PAB.

           IF  WRK-CTLIN GREATER 60
               PERFORM  4000-IMPRIME-CABEC.

           MOVE WRK-GRAVACAO TO WRK-OPERACAO.
           WRITE  REG-RELSAI  FROM  DETALHE.

           ADD  1  TO WRK-CTLIN.
           MOVE ' ' TO CARRO-DETALHE.
           PERFORM 1004-TESTE-RELSAI.


       3200-IMPRIME-99-FIM. EXIT.

       4000-IMPRIME-CABEC SECTION.

           MOVE WRK-GRAVACAO TO WRK-OPERACAO.

           ADD 1 TO WRK-CTA-PAG.
           MOVE WRK-CTA-PAG TO CABEC01-PAGINA.

           MOVE CABEC01 TO REG-LINHA
           WRITE REG-RELSAI.
           PERFORM 1004-TESTE-RELSAI.

           MOVE CABEC02 TO REG-LINHA.
           WRITE REG-RELSAI.
           PERFORM 1004-TESTE-RELSAI.

           MOVE CABEC03 TO REG-LINHA.
           WRITE REG-RELSAI.
           PERFORM 1004-TESTE-RELSAI.

           MOVE CABEC04 TO REG-LINHA.
           WRITE REG-RELSAI.
           PERFORM 1004-TESTE-RELSAI.

           MOVE CABEC05 TO REG-LINHA.
           WRITE REG-RELSAI.
           PERFORM 1004-TESTE-RELSAI.

           MOVE 10 TO WRK-CTLIN.
           MOVE '0' TO CARRO-DETALHE.

       4000-IMPR-CABEC-FIM. EXIT.

       4100-IMPRIME-TOTAL-PAB SECTION.

           ADD  2  TO  WRK-CTLIN.
           IF  WRK-CTLIN GREATER 60
               PERFORM  4000-IMPRIME-CABEC.
           MOVE WRK-GRAVACAO TO WRK-OPERACAO.
           WRITE REG-RELSAI  FROM TOTAL2.
           PERFORM 1004-TESTE-RELSAI.


       4100-IMPRIME-99-FIM.  EXIT.

       4110-IMPRIME-TOTAL-AGENCIA  SECTION.

           ADD  2  TO  WRK-CTLIN.
           IF  WRK-CTLIN GREATER 60
               PERFORM  4000-IMPRIME-CABEC.

           MOVE WRK-GRAVACAO TO WRK-OPERACAO.
           WRITE REG-RELSAI  FROM TOTAL1.
           PERFORM 1004-TESTE-RELSAI.

       4110-IMPRIME-99-FIM.  EXIT.

       4120-IMPRIME-CABEC-PAB SECTION.

           ADD  2  TO  WRK-CTLIN.
           IF  WRK-CTLIN GREATER 60
               PERFORM  4000-IMPRIME-CABEC
           ELSE
               MOVE WRK-GRAVACAO TO WRK-OPERACAO
               WRITE REG-RELSAI  FROM CABEC04
               PERFORM 1004-TESTE-RELSAI.

       4120-IMPRIME-99-FIM.  EXIT.

       5000-IMPRIME-RELSAI2  SECTION.

           IF  WRK-CTLIN2 GREATER 60
               MOVE WRK-GRAVACAO TO WRK-OPERACAO

               ADD 1 TO WRK-CTA-PAG2
               MOVE WRK-CTA-PAG2 TO CABEC01-PAGINAA

               MOVE CABEC01-A TO REG-LINHA2
               WRITE REG-RELSAI2
               PERFORM 1004-TESTE-RELSAI

               MOVE CABEC02 TO REG-LINHA2
               WRITE REG-RELSAI2
               PERFORM 1004-TESTE-RELSAI

               MOVE CABEC03A TO REG-LINHA2
               WRITE REG-RELSAI2
               PERFORM 1004-TESTE-RELSAI
               MOVE 4 TO WRK-CTLIN2.

           MOVE  AGEN-TAB-TAXA  (IND)   TO LIN-AGENCIA.
           MOVE  PAB-TAB-TAXA   (IND)   TO LIN-PAB2.
           MOVE  TAXA-TAB-TAXA  (IND)   TO LIN-TAXA2.
           MOVE  PERC-TAB-TAXA  (IND)   TO LIN-PERCENT2.
           MOVE  QTDE-TAB-TAXA  (IND)   TO LIN-QTDE.

           WRITE REG-RELSAI2 FROM DETALHE2
           PERFORM 1005-TESTE-RELSAI2
           ADD  2  TO WRK-CTLIN2.
           ADD  1  TO IND.

       5000-FIM-RELSAI2. EXIT.

       6000-EMITIR-DISPLAY-NAO-ENC  SECTION.

           DISPLAY ' '
           DISPLAY ' **************** GFCT1992 *******************'
           DISPLAY
                   ' *   CHAVE CADCLIE NAO ENCONTRADA NO GFCTB054 *'
           DISPLAY ' *                                           *'
           DISPLAY ' *   CHAVE CADCLIE = ' CHAVE-CADCLIE
      -            '              *'
           DISPLAY ' *   CHAVE GFCTB054 = ' CHAVE-GFCTB054
      -            '              *'
           DISPLAY ' **************** GFCT1992 *******************'
           DISPLAY '                                           '.

       6000-EMITIR-99-FIM.  EXIT.

       9000-PROCEDIMENTOS-FINAIS SECTION.

           IF ACUMULADOR-PAB  NOT EQUAL  ZEROS
               MOVE WRK-PAB-ANTERIOR     TO CABEC04-POSTO
                                            TOTAL2-CODPAB
               MOVE ACUMULADOR-PAB       TO TOTAL2-PAB
               PERFORM 4100-IMPRIME-TOTAL-PAB.

           IF ACUMULADOR-AGENCIA NOT EQUAL ZEROS
              MOVE WRK-AGENCIA-ANTERIOR TO CABEC03-AGENCIA
                                           TOTAL1-CODAGEN
              MOVE ACUMULADOR-AGENCIA   TO TOTAL1-AGENCIA
              PERFORM 4110-IMPRIME-TOTAL-AGENCIA.

           ADD  4   TO  WRK-CTLIN.

           IF  WRK-CTLIN  GREATER 60
               MOVE WRK-GRAVACAO TO WRK-OPERACAO

               ADD 1 TO WRK-CTA-PAG
               MOVE WRK-CTA-PAG TO CABEC01-PAGINA

               MOVE CABEC01 TO REG-LINHA
               WRITE REG-RELSAI
               PERFORM 1004-TESTE-RELSAI

               MOVE CABEC02 TO REG-LINHA
               WRITE REG-RELSAI
               PERFORM 1004-TESTE-RELSAI.

           MOVE WRK-GRAVACAO TO WRK-OPERACAO.
           MOVE TOTAL-AGENCIA TO TOTAL-AGENCIA-T.
           MOVE TOTAL-PABS    TO TOTAL-PAB-T.

           WRITE REG-RELSAI FROM TOTAL-GERAL-AG.
           WRITE REG-RELSAI FROM TOTAL-GERAL-PAB.

           DISPLAY ' '.
           DISPLAY
           '********************** GFCT1992 ****************************
      -    '*****'.
           DISPLAY
           '*
      -    '    *'.
           MOVE WS-LIDOS-CADCLIE  TO WS-DISPLAY-EDITADO.
           DISPLAY
           '* TOTAL REGISTROS LIDOS NO CADCLIE .................: '
                                              WS-DISPLAY-EDITADO ' *'.
           MOVE WS-LIDOS-GFCTB054  TO WS-DISPLAY-EDITADO.
           DISPLAY
           '* TOTAL REGISTROS LIDOS NO GFCTB054 ................: '
                                              WS-DISPLAY-EDITADO ' *'.
           MOVE WS-GRAVA-CADTARIF TO WS-DISPLAY-EDITADO.
           DISPLAY
           '* TOTAL REGISTROS GRAVADOS CADTARIF ................: '
                                              WS-DISPLAY-EDITADO ' *'
           DISPLAY
           '*
      -    '    *'.
           DISPLAY
           '********************** GFCT1992 ****************************
      -    '*****'.
           DISPLAY ' '.



           CLOSE CADCLIE,
                 GFCTB054,
                 CADTARIF,
                 RELSAI
                 RELSAI2.

           MOVE WRK-FECHAMENTO TO WRK-OPERACAO.
           PERFORM 1000-TESTAR-FILE-STATUS.

           STOP RUN.

       9000-PROCEDIMENTOS-FINAIS-FIM. EXIT.
