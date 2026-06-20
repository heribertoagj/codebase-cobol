      *===============================================================*
       IDENTIFICATION                  DIVISION.
      *===============================================================*

       PROGRAM-ID. SLIG6801.
       AUTHOR.     FERNANDO BATISTI.
      *===============================================================*
      *                    C P M   S I S T E M A S                    *
      *---------------------------------------------------------------*
      *    PROGRAMA....: SLIG6801                                     *
      *    PROGRAMADOR.: FERNANDO BATISTI           - CPM PATO BRANCO *
      *    ANALISTA CPM: EDINA FATINA GNOATO        - CPM PATO BRANCO *
      *    ANALISTA....: ELIETE RALDI               - DTS - GRUPO 81  *
      *    DATA........: 23/03/2004                                   *
      *---------------------------------------------------------------*
      *    OBJETIVO....: ATUALIZAR A TABELA DE DEPENDENCIA BANDA      *
      *      MAGNETICA.                                               *
      *---------------------------------------------------------------*
      *    ARQUIVOS....:                                              *
      *                 DDNAME                     INCLUDE/BOOK       *
      *                    DEPBANSR                   I#SLIG43        *
BRQ250*                    DEPBANDA                   I#SLIG5B        *
      *                    SRELCTRL                       -           *
      *---------------------------------------------------------------*
      *    INC'S.......:                                              *
      *    POL7100C - AREA DE TRATAMENTO DE ERRO PELA POOL7100        *
      *---------------------------------------------------------------*
      *    MODULOS.....:                                              *
      *    POOL7100 - MODULO DE TRATAMENTO DE ERRO                    *
      *    POOL7600 - OBTEM DATA E HORA DO SISTEMA                    *
      *===============================================================*
      *                    A L T E R A C A O                          *
      *---------------------------------------------------------------*
      *    PROGRAMADOR.: FERNANDO BATISTI           - CPM PATO BRANCO *
      *    ANALISTA CPM: EDINA FATINA GNOATO        - CPM PATO BRANCO *
      *    ANALISTA....: ELIETE RALDI               - DTS - GRUPO 81  *
      *    DATA........: 30/03/2004                                   *
      *---------------------------------------------------------------*
      *    OBJETIVO....: RETIRAR ACUMULACAO E GRAVAR CADA REGISTRO    *
      *      LIDO.                                                    *
      *---------------------------------------------------------------*
BRQ250* 25/02/2013  - ALTERACAO NO TRATAMENTO DO EMITENTE             *
BRQ250*               PASSA A SER PELO CODIGO DO CNPJ/CPF     LETO    *
      *===============================================================*

      *===============================================================*
       ENVIRONMENT                     DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *---------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *---------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *---------------------------------------------------------------*

       FILE-CONTROL.

           SELECT DEPBANSR ASSIGN      TO UT-S-DEPBANSR
           FILE STATUS                 IS WRK-FS-DEPBANSR.

           SELECT DEPBANDA ASSIGN      TO UT-S-DEPBANDA
           FILE STATUS                 IS WRK-FS-DEPBANDA.

           SELECT SRELCTRL ASSIGN      TO UT-S-SRELCTRL
           FILE STATUS                 IS WRK-FS-SRELCTRL.


      *===============================================================*
       DATA                            DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE                            SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *    INPUT:  ARQUIVO CONSOLIDADO EMITENTE                       *
      *            ORG. SEQUENCIAL     -   LRECL   =   300            *
      *---------------------------------------------------------------*

       FD  DEPBANSR
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#SLIG43'.


      *---------------------------------------------------------------*
      *    OUTPUT: ARQUIVO CHEQUE DEPDC-BANDA-MAGNETICA               *
      *            ORG. SEQUENCIAL     -   LRECL   =   126            *
      *---------------------------------------------------------------*

       FD  DEPBANDA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

BRQ250* ***   I#SLIG5B CRIADA A PARTIR DA I#SLIG52
BRQ250* ***   INCLUIDOS: FILIAL E CONTROLE DO CNPJ/CPF
       COPY 'I#SLIG5B'.


      *---------------------------------------------------------------*
      *    OUTPUT: RELATORIO DE CONTROLE                              *
      *            ORG. SEQUENCIAL     -   LRECL   =   080            *
      *---------------------------------------------------------------*

       FD  SRELCTRL
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-SRELCTRL             PIC  X(080).


      *---------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** SLIG6801 - INICIO DA AREA DE WORKING ***'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** ACUMULADORES ***'.
      *---------------------------------------------------------------*

       77  ACU-LIDOS-DEPBANSR         PIC  9(009) COMP-3   VALUE ZEROS.
       77  ACU-GRAVA-DEPBANDA         PIC  9(009) COMP-3   VALUE ZEROS.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS DE FILE-STATUS ***'.
      *---------------------------------------------------------------*

       77  WRK-FS-DEPBANSR             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-DEPBANDA             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SRELCTRL             PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.


      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE AUXILIARES ***'.
      *---------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)        VALUE 'BATCH'.

       77  WRK-MSG02                   PIC  X(075)        VALUE
           '** ARQUIVO DEPBANSR FORA DE SEQUENCIA **'.

       01  WRK-CARTEIRA-N3               PIC  9(003)       VALUE ZEROS.
       01  WRK-CARTEIRA-N3-R             REDEFINES
           WRK-CARTEIRA-N3.
           03 FILLER                     PIC  X(001).
           03 WRK-CARTEIRA-N2            PIC  9(002).

       01  WRK-CHV-DEPBANSR-ATU.
           03  WRK-BANCO-CLI-ATU       PIC  9(003)        VALUE ZEROS.
           03  WRK-AGENCIA-CLI-ATU     PIC  9(005)        VALUE ZEROS.
           03  WRK-BANCO-EMI-ATU       PIC  9(003)        VALUE ZEROS.
           03  WRK-AGENCIA-EMI-ATU     PIC  9(005)        VALUE ZEROS.
           03  WRK-CONTA-EMI-ATU       PIC  9(013)        VALUE ZEROS.
           03  WRK-CARTEIRA-EMI-ATU    PIC  9(003)        VALUE ZEROS.

       01  WRK-CHV-DEPBANSR-ANT.
           03  WRK-BANCO-CLI-ANT       PIC  9(003)        VALUE ZEROS.
           03  WRK-AGENCIA-CLI-ANT     PIC  9(005)        VALUE ZEROS.
           03  WRK-BANCO-EMI-ANT       PIC  9(003)        VALUE ZEROS.
           03  WRK-AGENCIA-EMI-ANT     PIC  9(005)        VALUE ZEROS.
           03  WRK-CONTA-EMI-ANT       PIC  9(013)        VALUE ZEROS.
           03  WRK-CARTEIRA-EMI-ANT    PIC  9(003)        VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ERROS DE FILE-STATUS ***'.
      *---------------------------------------------------------------*

       01  WRK-ERRO-TEXTO.
           03  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           03  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           03  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           03  WRK-NOME-ARQ            PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(018)         VALUE
               ' - FILE STATUS = '.
           03  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE ' **'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA POOL7600 ***'.
      *---------------------------------------------------------------*

       01  WRK-DATA-HORA.
           03  WRK-DATA-JULIANA        PIC  9(005) COMP-3  VALUE ZEROS.
           03  WRK-DATA-AAMMDD         PIC  9(007) COMP-3  VALUE ZEROS.
           03  WRK-DATA-AAAAMMDD       PIC  9(009) COMP-3  VALUE ZEROS.
           03  WRK-HORA-HHMMSS         PIC  9(007) COMP-3  VALUE ZEROS.
           03  WRK-HORA-HHMMSSMMMMMM   PIC  9(013) COMP-3  VALUE ZEROS.
           03  WRK-TIMESTAMP.
               05  WRK-ANO             PIC  9(004)         VALUE ZEROS.
               05  WRK-MES             PIC  9(002)         VALUE ZEROS.
               05  WRK-DIA             PIC  9(002)         VALUE ZEROS.
               05  FILLER              PIC  X(012)         VALUE SPACES.


      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO RELATORIO SRELCTRL ***'.
      *---------------------------------------------------------------*

       01  CABEC1.
           03  FILLER                  PIC  X(001)         VALUE '1'.
           03  FILLER                  PIC  X(023)         VALUE
               'BRADESCO'.
           03  FILLER                  PIC  X(046)         VALUE
               'SLIG - SISTEMA DE LIQUIDEZ GERAL'.
           03  FILLER                  PIC  X(011)         VALUE
               'FOL. UNICA'.

       01  CABEC2.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(069)         VALUE
               'SLIG6801'.
           03  CB2-DIA                 PIC  99             VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB2-MES                 PIC  99             VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB2-ANO                 PIC  9999           VALUE ZEROS.

       01  CABEC3.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE SPACES.
           03  FILLER                  PIC  X(039)         VALUE
               'ATUALIZACAO DA TABELA DEPDC_BANDA_MAGNT'.

       01  CABEC4.
           03  FILLER                  PIC  X(001)         VALUE '-'.
           03  FILLER                  PIC  X(026)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'CONTROLE OPERACIONAL'.

       01  LINDET1.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(009)         VALUE SPACES.
           03  FILLER                  PIC  X(035)         VALUE
               'REGISTROS LIDOS      (DEPBANSR) :  '.
           03  LD1-LIDOS-DEPBANSR      PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.

       01  LINDET2.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(009)         VALUE SPACES.
           03  FILLER                  PIC  X(035)         VALUE
               'REGISTROS GRAVADOS   (DEPBANDA) :  '.
           03  LD2-GRAVA-DEPBANDA      PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.


      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA POOL7100 ***'.
      *---------------------------------------------------------------*

       COPY POL7100C.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** SLIG6801 - FIM DA AREA DE WORKING ***'.
      *---------------------------------------------------------------*

      *===============================================================*
       PROCEDURE                       DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *---------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR
             UNTIL WRK-FS-DEPBANSR     EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *---------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *---------------------------------------------------------------*

           OPEN INPUT  DEPBANSR
                OUTPUT DEPBANDA
                       SRELCTRL.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           CALL 'POOL7600'             USING WRK-DATA-HORA.

           MOVE WRK-DIA                TO CB2-DIA.
           MOVE WRK-MES                TO CB2-MES.
           MOVE WRK-ANO                TO CB2-ANO.

      *---------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *---------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-DEPBANSR.

           PERFORM 1120-TESTAR-FS-DEPBANDA.

           PERFORM 1130-TESTAR-FS-SRELCTRL.

      *---------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1110-TESTAR-FS-DEPBANSR         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-DEPBANSR         NOT EQUAL ZEROS
               MOVE 'DEPBANSR'         TO WRK-NOME-ARQ
               MOVE WRK-FS-DEPBANSR    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1120-TESTAR-FS-DEPBANDA         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-DEPBANDA         NOT EQUAL ZEROS
               MOVE 'DEPBANDA'         TO WRK-NOME-ARQ
               MOVE WRK-FS-DEPBANDA    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1130-TESTAR-FS-SRELCTRL         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-SRELCTRL         NOT EQUAL ZEROS
               MOVE 'SRELCTRL'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SRELCTRL    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *---------------------------------------------------------------*

           PERFORM 2100-LER-DEPBANSR.

           IF WRK-FS-DEPBANSR          EQUAL '10'
              DISPLAY '************** SLIG6801 ****************'
              DISPLAY '*                                      *'
              DISPLAY '*       ARQUIVO DEPBANSR VAZIO         *'
              DISPLAY '*                                      *'
              DISPLAY '************** SLIG6801 ****************'
           END-IF.

      *---------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2100-LER-DEPBANSR               SECTION.
      *---------------------------------------------------------------*

           READ DEPBANSR.

           IF  WRK-FS-DEPBANSR          EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA             TO WRK-OPERACAO.
           PERFORM 1110-TESTAR-FS-DEPBANSR.

           MOVE SLIG43-BCO-CLI         TO WRK-BANCO-CLI-ATU.
           MOVE SLIG43-AGENCIA-CLI     TO WRK-AGENCIA-CLI-ATU.
           MOVE SLIG43-BCO-EMITT       TO WRK-BANCO-EMI-ATU.
           MOVE SLIG43-AGENCIA-EMITT   TO WRK-AGENCIA-EMI-ATU.
           MOVE SLIG43-CONTA-EMITT     TO WRK-CONTA-EMI-ATU.
           MOVE SLIG43-CARTEIRA        TO WRK-CARTEIRA-EMI-ATU

           ADD 1                        TO ACU-LIDOS-DEPBANSR.

           IF  WRK-CHV-DEPBANSR-ATU     LESS WRK-CHV-DEPBANSR-ANT
               PERFORM 2110-EMITIR-ERRO-DEPBANSR
           END-IF.

      *---------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2110-EMITIR-ERRO-DEPBANSR       SECTION.
      *---------------------------------------------------------------*

           DISPLAY '************** SLIG6801 ****************'
           DISPLAY '*                                      *'
           DISPLAY '*  ARQUIVO DEPBANSR FORA DE SEQUENCIA  *'
           DISPLAY '*                                      *'
           DISPLAY '* CHAVE ANTERIOR:                      *'
           DISPLAY '* SLIG43-BCO-CLI       = ' WRK-BANCO-CLI-ANT
                                               '           *'
           DISPLAY '* SLIG43-AGENCIA-CLI   = ' WRK-AGENCIA-CLI-ANT
                                                 '         *'
           DISPLAY '* SLIG43-BCO-EMITT     = ' WRK-BANCO-EMI-ANT
                                               '           *'
           DISPLAY '* SLIG43-AGENCIA-EMITT = ' WRK-AGENCIA-EMI-ANT
                                                 '         *'
           DISPLAY '* SLIG43-CONTA-EMITT   = ' WRK-CONTA-EMI-ANT
                                                         ' *'
           DISPLAY '* SLIG43-CARTEIRA      = ' WRK-CARTEIRA-EMI-ANT
                                               '           *'
           DISPLAY '*                                      *'
           DISPLAY '* CHAVE ATUAL:                         *'
           DISPLAY '* SLIG43-BCO-CLI       = ' WRK-BANCO-CLI-ATU
                                               '           *'
           DISPLAY '* SLIG43-AGENCIA-CLI   = ' WRK-AGENCIA-CLI-ATU
                                                 '         *'
           DISPLAY '* SLIG43-BCO-EMITT     = ' WRK-BANCO-EMI-ATU
                                               '           *'
           DISPLAY '* SLIG43-AGENCIA-EMITT = ' WRK-AGENCIA-EMI-ATU
                                                 '         *'
           DISPLAY '* SLIG43-CONTA-EMITT   = ' WRK-CONTA-EMI-ATU
                                                         ' *'
           DISPLAY '* SLIG43-CARTEIRA      = ' WRK-CARTEIRA-EMI-ATU
                                               '           *'
           DISPLAY '*                                      *'
           DISPLAY '************** SLIG6801 ****************'

           MOVE WRK-MSG02              TO ERR-TEXTO.

           PERFORM 9999-PROCESSAR-ROTINA-ERRO.

      *---------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *---------------------------------------------------------------*

           PERFORM 3100-GRAVAR-DEPBANDA.

           MOVE WRK-CHV-DEPBANSR-ATU   TO WRK-CHV-DEPBANSR-ANT.

           PERFORM 2100-LER-DEPBANSR.

      *---------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3100-GRAVAR-DEPBANDA            SECTION.
      *---------------------------------------------------------------*

           INITIALIZE REG-DEPDC-BANDA-MAGNT.

           MOVE WRK-BANCO-CLI-ATU       TO SLIG5B-EMPR-INC.
           MOVE WRK-AGENCIA-CLI-ATU     TO SLIG5B-DEPENDENCIA.
           MOVE WRK-BANCO-EMI-ATU       TO SLIG5B-BCO-SACDO.
           MOVE WRK-AGENCIA-EMI-ATU     TO SLIG5B-AGEN-SACDO.
           MOVE WRK-CONTA-EMI-ATU       TO SLIG5B-CONTA-SACDO.

CNPJ7C     MOVE SLIG43-CGC-CPF-EMITT       TO SLIG5B-CGC-CPF-SACDO
CNPJ7C     MOVE SLIG43-FILIAL-EMITT        TO SLIG5B-CFLIAL-SACDO
CNPJ7C     MOVE SLIG43-CONTROLE-EMITT      TO SLIG5B-CCTRL-SACDO

           MOVE SLIG43-CGC-CPF-EMITT-ST  TO SLIG5B-CGC-CPF-SACDO-ST.
CNPJ7C     MOVE ZEROS                    TO SLIG5B-CFLIAL-SACDO-ST
CNPJ7C     MOVE SLIG43-FILIAL-EMITT-ST   TO SLIG5B-CFLIAL-SACDO-ST(2:4)
BRQ250     MOVE SLIG43-CONTROLE-EMITT-ST TO SLIG5B-CCTRL-SACDO-ST
           MOVE SLIG43-TIPO-PESSOA-EMITT
                                        TO SLIG5B-INDCD-PSSOA-SACDO.
           MOVE WRK-CARTEIRA-EMI-ATU    TO WRK-CARTEIRA-N3.
           MOVE WRK-CARTEIRA-N2         TO SLIG5B-IDTFD-CART-LIQDZ.
           MOVE SLIG43-VCTO-PER-PG      TO SLIG5B-VCTO-PER-PG.
           MOVE SLIG43-VCTO-PER-NAO-PG  TO SLIG5B-VCTO-PER-NAO-PG.
           MOVE SLIG43-VCTO-PER-BXADO   TO SLIG5B-VCTO-PER-BXADO.
           MOVE SLIG43-VCTO-APOS-PER-PG
                                        TO SLIG5B-VCTO-APOS-PER-PG.
           MOVE SLIG43-VCTO-APOS-PER-NAO-PG
                                        TO SLIG5B-VCTO-APOS-PER-NAO-PG.
           MOVE SLIG43-VCTO-APOS-PER-BXADO
                                        TO SLIG5B-VCTO-APOS-PER-BXADO.
           MOVE SLIG43-CHEQ-PRIM-DEVLC  TO SLIG5B-CHEQ-PRIM-DEVLC.
           MOVE SLIG43-CHEQ-REAP        TO SLIG5B-CHEQ-REAP.
           MOVE SLIG43-CHEQ-REAP-DEVOL  TO SLIG5B-CHEQ-REAP-DEVOL.
           MOVE SLIG43-CHEQ-REAP-PG     TO SLIG5B-CHEQ-REAP-PG.
           MOVE SLIG43-CHEQ-DEVOL-ANTER TO SLIG5B-CHEQ-DEVOL-ANTER.
           MOVE WRK-GRAVACAO            TO WRK-OPERACAO.

           WRITE REG-DEPDC-BANDA-MAGNT.

           PERFORM 1120-TESTAR-FS-DEPBANDA.

           ADD 1                       TO ACU-GRAVA-DEPBANDA.

      *---------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *---------------------------------------------------------------*

           PERFORM 4100-IMPRIMIR-SRELCTRL.

           CLOSE DEPBANSR
                 DEPBANDA
                 SRELCTRL.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           GOBACK.

      *---------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4100-IMPRIMIR-SRELCTRL          SECTION.
      *---------------------------------------------------------------*

           MOVE ACU-LIDOS-DEPBANSR     TO LD1-LIDOS-DEPBANSR.
           MOVE ACU-GRAVA-DEPBANDA     TO LD2-GRAVA-DEPBANDA.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE FD-REG-SRELCTRL       FROM CABEC1.
           PERFORM 1130-TESTAR-FS-SRELCTRL.

           WRITE FD-REG-SRELCTRL       FROM CABEC2.
           PERFORM 1130-TESTAR-FS-SRELCTRL.

           WRITE FD-REG-SRELCTRL       FROM CABEC3.
           PERFORM 1130-TESTAR-FS-SRELCTRL.

           WRITE FD-REG-SRELCTRL       FROM CABEC4.
           PERFORM 1130-TESTAR-FS-SRELCTRL.

           WRITE FD-REG-SRELCTRL       FROM LINDET1.
           PERFORM 1130-TESTAR-FS-SRELCTRL.

           WRITE FD-REG-SRELCTRL       FROM LINDET2.
           PERFORM 1130-TESTAR-FS-SRELCTRL.

      *---------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *---------------------------------------------------------------*

           MOVE 'APL'                  TO ERR-TIPO-ACESSO.
           MOVE 'SLIG6801'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *---------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
