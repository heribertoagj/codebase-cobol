      *===============================================================*
       IDENTIFICATION                  DIVISION.
      *===============================================================*

       PROGRAM-ID. SLIG6420.
       AUTHOR.     LUIS VIACELLI FERNANDES.
      *===============================================================*
      *                    C P M  -  S I S T E M A S                  *
      *---------------------------------------------------------------*
      *    PROGRAMA....:   SLIG6420                                   *
      *    PROGRAMADOR.:   LUIS VIACELLI FERNANDES -  CPM PATO BRANCO *
      *    ANALISTA CPM:   EDINA FATIMA GNOATO     -  CPM PATO BRANCO *
      *    ANALISTA....:   TERRY                   -  DTS / GRUPO 75  *
      *    DATA........:   30/03/2004                                 *
      *---------------------------------------------------------------*
      *    OBJETIVO....:   UNIFICA OS ARQUIVOS DE MOVIMENTO DE PAGA-  *
      *      MENTO, DE MOVIMENTO DE BAIXA  E  DE MOVIMENTO DE FINAN-  *
      *      CEIRO ATIVO.                                             *
      *---------------------------------------------------------------*
      *    ARQUIVOS....:                                              *
      *                DDNAME                      INCLUDE/BOOK       *
      *                    PGTODECE                  I#SLIG46         *
      *                    BAXADECE                  I#SLIG46         *
      *                    FINVNDOR                  I#SLIG64         *
      *                    COMPVDOR                  I#SLIG31         *
      *                    SRELCTRL                     -             *
      *---------------------------------------------------------------*
      *    INC'S.......:                                              *
      *    POL7100C - AREA PARA TRATAMENTO DE ERROS PELA POOL7100.    *
      *    I#MESUM4 - AREA DE PARAMETROS DO MODULO MESU9014.          *
      *    I#GRUP34 - INC UTILIZADA PARA OBTER O GRUPO ATRAVES DO CNPJ*
      *---------------------------------------------------------------*
      *    MODULOS.....:                                              *
      *    GRUP0930 - CONSULTAR A EMPRESA/PESSOA FISICA POR 'GRUPO    *
      *               E CGC' OU SO POR 'CGC'.                         *
      *    MESU9023 - OBTER CODIGO DA EMPRESA, DEPARTAMENTO DE DETER- *
      *               MINADO TIPO DE RELACIONAMENTO E NOME DO MESMO.  *
      *    MESU9014 - MODULO PARA OBTER DADOS DA DEPENDENCIA.         *
      *    POOL7100 - TRATAR ERROS DO PROGRAMA.                       *
      *    POOL7600 - OBTER DATA DO SISTEMA.                          *
      *===============================================================*
           EJECT
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

           SELECT PGTODECE ASSIGN      TO UT-S-PGTODECE
           FILE STATUS                 IS WRK-FS-PGTODECE.

           SELECT BAXADECE ASSIGN      TO UT-S-BAXADECE
           FILE STATUS                 IS WRK-FS-BAXADECE.

           SELECT FINVNDOR ASSIGN      TO UT-S-FINVNDOR
           FILE STATUS                 IS WRK-FS-FINVNDOR.

           SELECT COMPVDOR ASSIGN      TO UT-S-COMPVDOR
           FILE STATUS                 IS WRK-FS-COMPVDOR.

           SELECT SRELCTRL ASSIGN      TO UT-S-SRELCTRL
           FILE STATUS                 IS WRK-FS-SRELCTRL.

           EJECT
      *===============================================================*
       DATA                            DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE                            SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE MOVIMENTO DE PAGAMENTO CONTRATO/PARCELA *
      *            DE VENDOR                                          *
      *            ORG. SEQUENCIAL     -   LRECL   =   300            *
      *---------------------------------------------------------------*

       FD  PGTODECE
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

-INC I#SLIG46

           EJECT
      *---------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE MOVIMENTO DE BAIXA CONTRATO/PARCELA     *
      *            DE VENDOR                                          *
      *            ORG. SEQUENCIAL     -   LRECL   =   300            *
      *---------------------------------------------------------------*

       FD  BAXADECE
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

-INC I#SLIG46

           EJECT
      *---------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE MOVIMENTO DE FINANCEIRO ATIVO           *
      *            CONTRATO/PARCELA DE VENDOR                         *
      *            ORG. SEQUENCIAL     -   LRECL   =   300            *
      *---------------------------------------------------------------*

       FD  FINVNDOR
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

-INC I#SLIG64

           EJECT
      *---------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE MOVIMENTO DE PAGAMENTO CONTRATO/PARCELA *
      *            DE VENDOR DECENAL                                  *
      *            ORG. SEQUENCIAL     -   LRECL   =   300            *
      *---------------------------------------------------------------*

       FD  COMPVDOR
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

-INC I#SLIG31

           EJECT
      *---------------------------------------------------------------*
      *    OUTPUT: RELATORIO DE CONTROLE                              *
      *            ORG. SEQUENCIAL     -   LRECL   =   081            *
      *---------------------------------------------------------------*

       FD  SRELCTRL
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-SRELCTRL                PIC  X(081).

      *---------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** SLIG6420 - INICIO DA AREA DE WORKING ***'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACUMULADORES ***'.
      *---------------------------------------------------------------*

       77  ACU-LIDOS-PGTODECE          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-BAXADECE          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-FINVNDOR          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-COMPVDOR          PIC  9(009) COMP-3  VALUE ZEROS.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA TESTE DE FILE STATUS ***'.
      *---------------------------------------------------------------*

       77  WRK-FS-PGTODECE             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-BAXADECE             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-FINVNDOR             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-COMPVDOR             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SRELCTRL             PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

           EJECT
      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VARIAVEIS AUXILIARES ***'.
      *---------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-AMBIENTE                PIC  X(001)         VALUE SPACES.
       77  WRK-IO-PCB                  PIC  X(032)         VALUE SPACES.
       77  WRK-ALT-PCB                 PIC  X(032)         VALUE SPACES.
       77  WRK-SQLCA                   PIC  X(136)         VALUE SPACES.
       77  WRK-ERRO-AREA               PIC  X(107)         VALUE SPACES.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS DE ERRO ***'.
      *---------------------------------------------------------------*

       01  WRK-MSG01                   PIC  X(075)         VALUE
           '*** ARQUIVO PGTODECE VAZIO ***'.
       01  WRK-MSG02                   PIC  X(075)         VALUE
           '*** ARQUIVO BAXADECE VAZIO ***'.
       01  WRK-MSG03                   PIC  X(075)         VALUE
           '*** ARQUIVO FINVNDOR VAZIO ***'.

       01  WRK-ERRO-MODULO.
           03  FILLER                  PIC  X(059)         VALUE
               '** ERRO NO ACESSO AO MODULO MESU9023 - CODIGO DE RETORNO
      -        ' = '.
           03  WRK-COD-RET-9023        PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(003)         VALUE
               ' **'.

       01  WRK-ERRO-0930.
           03  FILLER                  PIC  X(059)         VALUE
               '** ERRO NO ACESSO AO MODULO GRUP0930 - CODIGO DE RETORNO
      -        ' = '.
           03  WRK-COD-RET-0930        PIC  9(003)         VALUE ZEROS.
           03  FILLER                  PIC  X(002)         VALUE
               '**'.

       01  WRK-ERRO-ARQUIVO.
           03  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           03  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           03  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           03  WRK-NOME-ARQUIVO        PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(017)         VALUE
               ' - FILE STATUS = '.
           03  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE ' **'.

           EJECT
      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA CABECALHOS ***'.
      *---------------------------------------------------------------*

       01  CABEC1.
           03  FILLER                  PIC  X(001)         VALUE '1'.
           03  FILLER                  PIC  X(023)         VALUE
               'BRADESCO'.
           03  FILLER                  PIC  X(047)         VALUE
               'SLIG - SISTEMA DE INDICE DE LIQUIDEZ'.
           03  FILLER                  PIC  X(010)         VALUE
               'FOL. UNICA'.

       01  CABEC2.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(070)         VALUE
               'SLIG6420'.
           03  CB2-DIA                 PIC  99.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB2-MES                 PIC  99.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB2-ANO                 PIC  9999.

       01  CABEC3.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE SPACES.
           03  FILLER                  PIC  X(046)         VALUE
               'UNIFICA ARQUIVOS PAGAMENTO, BAIXA E FINANCEIRO'.

       01  CABEC4.
           03  FILLER                  PIC  X(001)         VALUE '-'.
           03  FILLER                  PIC  X(033)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'CONTROLE OPERACIONAL'.

           EJECT
      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA LINHAS DE TOTAIS ***'.
      *---------------------------------------------------------------*

       01  LINTOT1.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(017)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'REGISTROS LIDOS'.
           03  FILLER                  PIC  X(013)         VALUE
               '(PGTODECE) : '.
           03  LT1-TOT-LIDOS-PGTODECE  PIC  ZZZ.ZZZ.ZZ9.

       01  LINTOT2.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(017)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'REGISTROS LIDOS'.
           03  FILLER                  PIC  X(013)         VALUE
               '(BAXADECE) : '.
           03  LT2-TOT-LIDOS-BAXADECE  PIC  ZZZ.ZZZ.ZZ9.

       01  LINTOT3.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(017)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'REGISTROS LIDOS'.
           03  FILLER                  PIC  X(013)         VALUE
               '(FINVNDOR) : '.
           03  LT3-TOT-LIDOS-FINVNDOR  PIC  ZZZ.ZZZ.ZZ9.

       01  LINTOT4.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(017)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'REGISTROS GRAVADOS'.
           03  FILLER                  PIC  X(013)         VALUE
               '(COMPVDOR) : '.
           03  LT4-TOT-GRAVA-COMPVDOR  PIC  ZZZ.ZZZ.ZZ9.

           EJECT
      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA POOL7600 ***'.
      *---------------------------------------------------------------*

       01  WRK-AREA-POOL7600.
           03  WRK-DT-JULIANA          PIC  9(005) COMP-3  VALUE ZEROS.
           03  WRK-DT-AAMMDD           PIC  9(007) COMP-3  VALUE ZEROS.
           03  WRK-DT-AAAAMMDD         PIC  9(009) COMP-3  VALUE ZEROS.
           03  WRK-TI-HHMMSS           PIC  9(007) COMP-3  VALUE ZEROS.
           03  WRK-TI-HHMMSSMMMMMM     PIC  9(013) COMP-3  VALUE ZEROS.
           03  WRK-TIMESTAMP.
               05  WRK-ANO-7600        PIC  9(004)         VALUE ZEROS.
               05  WRK-MES-7600        PIC  9(002)         VALUE ZEROS.
               05  WRK-DIA-7600        PIC  9(002)         VALUE ZEROS.
               05  FILLER              PIC  X(012)         VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MODULO MESU9023 ***'.
      *---------------------------------------------------------------*

       01  WRK-PARM.
           03  WRK-ENTRADA.
               05  WRK-ENT-NOME-PGM    PIC  X(008)         VALUE SPACES.
               05  WRK-ENT-COD-RETORNO PIC  9(002)         VALUE ZEROS.
               05  WRK-ENT-TPO-RELAC   PIC  9(003)         VALUE ZEROS.
               05  WRK-ENT-BANCO       PIC  9(005)         VALUE ZEROS.
               05  WRK-ENT-AGENCIA     PIC  9(005)         VALUE ZEROS.
           03  WRK-SAIDA.
               05  WRK-SAI-EMPR-RELAC  PIC  9(005)         VALUE ZEROS.
               05  WRK-SAI-DEPTO-RELAC PIC  9(005)         VALUE ZEROS.
               05  WRK-SAI-NOME-DEPDC  PIC  X(050)         VALUE SPACES.

           EJECT
      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MODULO MESU9014 ***'.
      *---------------------------------------------------------------*

-INC I#MESUM4

           EJECT
      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MODULO GRUP0930 ***'.
      *---------------------------------------------------------------*

       COPY GRUPW01S.

           EJECT
      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA POOL7100 ***'.
      *---------------------------------------------------------------*

-INC POL7100C

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** SLIG6420 - FIM DA AREA DE WORKING ***'.
      *---------------------------------------------------------------*
           EJECT
      *===============================================================*
       PROCEDURE                       DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       000000-INICIAR                  SECTION.
      *---------------------------------------------------------------*

           PERFORM 100000-INICIALIZAR.

           PERFORM 200000-VERIFICAR-VAZIO.

           PERFORM 300000-PROCESSAR.

           PERFORM 400000-FINALIZAR.

      *---------------------------------------------------------------*
       000000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.
      *---------------------------------------------------------------*

           OPEN INPUT  PGTODECE
                       BAXADECE
                       FINVNDOR
                OUTPUT COMPVDOR
                       SRELCTRL.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 110000-TESTAR-FILE-STATUS.

      *---------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       110000-TESTAR-FILE-STATUS       SECTION.
      *---------------------------------------------------------------*

           PERFORM 111000-TESTAR-FS-PGTODECE.

           PERFORM 112000-TESTAR-FS-BAXADECE.

           PERFORM 113000-TESTAR-FS-FINVNDOR.

           PERFORM 114000-TESTAR-FS-COMPVDOR.

           PERFORM 115000-TESTAR-FS-SRELCTRL.

      *---------------------------------------------------------------*
       110000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       111000-TESTAR-FS-PGTODECE       SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-PGTODECE         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE  WRK-FS-PGTODECE   TO WRK-FILE-STATUS
               MOVE 'PGTODECE'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       111000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       112000-TESTAR-FS-BAXADECE       SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-BAXADECE         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE  WRK-FS-BAXADECE   TO WRK-FILE-STATUS
               MOVE 'BAXADECE'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       112000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       113000-TESTAR-FS-FINVNDOR       SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-FINVNDOR         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE  WRK-FS-FINVNDOR   TO WRK-FILE-STATUS
               MOVE 'FINVNDOR'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       113000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       114000-TESTAR-FS-COMPVDOR       SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-COMPVDOR         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE  WRK-FS-COMPVDOR   TO WRK-FILE-STATUS
               MOVE 'COMPVDOR'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       114000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       115000-TESTAR-FS-SRELCTRL       SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-SRELCTRL         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE  WRK-FS-SRELCTRL   TO WRK-FILE-STATUS
               MOVE 'SRELCTRL'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       115000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       200000-VERIFICAR-VAZIO          SECTION.
      *---------------------------------------------------------------*

           PERFORM 210000-LER-PGTODECE.

           IF  WRK-FS-PGTODECE         EQUAL '10'
               DISPLAY '***************** SLIG6420 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*          ARQUIVO PGTODECE VAZIO          *'
               DISPLAY '*                                          *'
               DISPLAY '*         PROCESSAMENTO  CANCELADO         *'
               DISPLAY '*                                          *'
               DISPLAY '***************** SLIG6420 *****************'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MSG01          TO ERR-TEXTO
VOLTAR         PERFORM 400000-FINALIZAR
VOLTAR*        P*RFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 220000-LER-BAXADECE.

           PERFORM 230000-LER-FINVNDOR.

           IF  WRK-FS-FINVNDOR         EQUAL '10'
               DISPLAY '***************** SLIG6420 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*          ARQUIVO FINVNDOR VAZIO          *'
               DISPLAY '*                                          *'
               DISPLAY '*         PROCESSAMENTO  CANCELADO         *'
               DISPLAY '*                                          *'
               DISPLAY '***************** SLIG6420 *****************'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MSG03          TO ERR-TEXTO
VOLTAR         PERFORM 400000-FINALIZAR
VOLTAR*        P*RFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       200000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       210000-LER-PGTODECE             SECTION.
      *---------------------------------------------------------------*

           READ PGTODECE.

           IF  WRK-FS-PGTODECE         EQUAL '10'
               GO                      TO 210000-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 111000-TESTAR-FS-PGTODECE.

           ADD 1                       TO ACU-LIDOS-PGTODECE.

      *---------------------------------------------------------------*
       210000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       220000-LER-BAXADECE             SECTION.
      *---------------------------------------------------------------*

           READ BAXADECE.

           IF  WRK-FS-BAXADECE         EQUAL '10'
               GO                      TO 220000-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 112000-TESTAR-FS-BAXADECE.

           ADD 1                       TO ACU-LIDOS-BAXADECE.

      *---------------------------------------------------------------*
       220000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       230000-LER-FINVNDOR             SECTION.
      *---------------------------------------------------------------*

           READ FINVNDOR.

           IF  WRK-FS-FINVNDOR         EQUAL '10'
               GO                      TO 230000-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 113000-TESTAR-FS-FINVNDOR.

           ADD 1                       TO ACU-LIDOS-FINVNDOR.

      *---------------------------------------------------------------*
       230000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       300000-PROCESSAR                SECTION.
      *---------------------------------------------------------------*

           PERFORM UNTIL WRK-FS-PGTODECE EQUAL '10'
             PERFORM 310000-FORMATAR-CAMPOS-PAGTO
             PERFORM 320000-GRAVAR-COMPVDOR
             PERFORM 210000-LER-PGTODECE
           END-PERFORM.

           PERFORM UNTIL WRK-FS-BAXADECE EQUAL '10'
             PERFORM 330000-FORMATAR-CAMPOS-BAXA
             PERFORM 320000-GRAVAR-COMPVDOR
             PERFORM 220000-LER-BAXADECE
           END-PERFORM.

           PERFORM UNTIL WRK-FS-FINVNDOR EQUAL '10'
             PERFORM 340000-FORMATAR-CAMPOS-FINAV
             PERFORM 320000-GRAVAR-COMPVDOR
             PERFORM 230000-LER-FINVNDOR
           END-PERFORM.

      *---------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       310000-FORMATAR-CAMPOS-PAGTO    SECTION.
      *---------------------------------------------------------------*

           INITIALIZE REG-VENDOR-COMPRADOR.

           MOVE SLIG46-CGC-CPF-COMP-ST   OF PGTODECE
                                         TO SLIG31-CGC-CPF-COMP-ST.
           MOVE SLIG46-FILIAL-COMP-ST    OF PGTODECE
                                         TO SLIG31-FILIAL-COMP-ST.
           MOVE SLIG46-CONTROLE-COMP-ST  OF PGTODECE
                                         TO SLIG31-CONTROLE-COMP-ST.
           MOVE SLIG46-TIPO-PESSOA-COMP  OF PGTODECE
                                         TO SLIG31-TIPO-PESSOA-COMP.
           MOVE SLIG46-BCO-COMP          OF PGTODECE
                                         TO SLIG31-BCO-COMP.
           MOVE SLIG46-AGENCIA-COMP      OF PGTODECE
                                         TO SLIG31-AGENCIA-COMP.
           MOVE SLIG46-CONTA-COMP        OF PGTODECE
                                         TO SLIG31-CONTA-COMP.
           MOVE SLIG46-NOME-COMP         OF PGTODECE
                                         TO SLIG31-NOME-COMP.
           MOVE SLIG46-CPRODT            OF PGTODECE
                                         TO SLIG31-CPRODT.
           MOVE SLIG46-CFAML OF PGTODECE TO SLIG31-CFAML.
           MOVE SLIG46-CCONTR            OF PGTODECE
                                         TO SLIG31-CCONTR.
           MOVE SLIG46-CSEQ  OF PGTODECE TO SLIG31-CSEQ.
           MOVE SLIG46-CPARCELA          OF PGTODECE
                                         TO SLIG31-CPARCELA.
           MOVE SLIG46-NUM-PAGTO         OF PGTODECE
                                         TO SLIG31-NUM-PAGTO.
           MOVE SLIG46-NOSSO-NUMERO      OF PGTODECE
                                         TO SLIG31-NOSSO-NUMERO.
           MOVE SLIG46-CGC-CPF-FORN-ST
           OF PGTODECE
                                         TO SLIG31-CGC-CPF-FORN-ST.
           MOVE SLIG46-FILIAL-FORN-ST
           OF PGTODECE
                                         TO SLIG31-FILIAL-FORN-ST.
           MOVE SLIG46-CONTROLE-FORN-ST
           OF PGTODECE
                                         TO SLIG31-CONTROLE-FORN-ST.
           MOVE SLIG46-TIPO-PESSOA-FORN  OF PGTODECE
                                         TO SLIG31-TIPO-PESSOA-FORN.
           MOVE SLIG46-BCO-FORN          OF PGTODECE
                                         TO SLIG31-BCO-FORN.
           MOVE SLIG46-AGENCIA-FORN      OF PGTODECE
                                         TO SLIG31-AGENCIA-FORN.
           MOVE SLIG46-CONTA-FORN        OF PGTODECE
                                         TO SLIG31-CONTA-FORN.
           MOVE SLIG46-NOME-FORN         OF PGTODECE
                                         TO SLIG31-NOME-FORN.
           MOVE SLIG46-DT-VENCTO-PARC    OF PGTODECE
                                         TO SLIG31-DT-VENCTO-PARC.
           MOVE SLIG46-DT-PAGTO-PARC     OF PGTODECE
                                         TO SLIG31-DT-PAGTO-PARC.
           MOVE SLIG46-CTPO-SIT-PARC     OF PGTODECE
                                         TO SLIG31-CPTO-SIT-PARC.
           MOVE SLIG46-DT-ENTRADA-ORIGEM OF PGTODECE
                                         TO SLIG31-DT-ENTRADA-ORIGEM.
           MOVE SLIG46-VL-PRINC-PARC     OF PGTODECE
                                         TO SLIG31-VL-PRINC-PARC.
           MOVE SLIG46-IND-PAGTO         OF PGTODECE
                                         TO SLIG31-IND-PAGTO.

      *---------------------------------------------------------------*
       310000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       320000-GRAVAR-COMPVDOR          SECTION.
      *---------------------------------------------------------------*

           PERFORM 321000-OBTER-DIR-REG.

           PERFORM 322000-OBTER-GER-REG.

           MOVE SPACES                 TO GRUPW01S-REG-EMPRESA.
           INITIALIZE                     GRUPW01S-REG-EMPRESA.

           MOVE '00'                   TO GRUPW01S-CGC-CPF(1:2).
           MOVE SLIG31-CGC-CPF-FORN-ST TO GRUPW01S-CGC-CPF(3:9).
           MOVE SLIG31-FILIAL-FORN-ST  TO GRUPW01S-FILIAL-CGC-CPF.
           MOVE SLIG31-CONTROLE-FORN-ST TO GRUPW01S-CTRL-CGC-CPF.
           MOVE ZEROS                  TO GRUPW01S-NUMERO-GRUPO-ENV.

           PERFORM 323000-OBTER-GRUPO-ECONM.

           IF  GRUPW01S-RETURN-CODE EQUAL ZEROS
               MOVE GRUPW01S-NUMERO-GRUPO-RET
                                          TO SLIG31-GRP-ECONOMICO-FORN
           ELSE
               MOVE ZEROS                 TO SLIG31-GRP-ECONOMICO-FORN
           END-IF.

           MOVE SPACES                 TO GRUPW01S-REG-EMPRESA.
           INITIALIZE                     GRUPW01S-REG-EMPRESA.

           MOVE '00'                    TO GRUPW01S-CGC-CPF(1:2)
           MOVE SLIG31-CGC-CPF-COMP-ST  TO GRUPW01S-CGC-CPF(3:9)
           MOVE SLIG31-FILIAL-COMP-ST   TO GRUPW01S-FILIAL-CGC-CPF.
           MOVE SLIG31-CONTROLE-COMP-ST TO GRUPW01S-CTRL-CGC-CPF.
           MOVE ZEROS                  TO GRUPW01S-NUMERO-GRUPO-ENV.

           PERFORM 323000-OBTER-GRUPO-ECONM.

           IF  GRUPW01S-RETURN-CODE  EQUAL ZEROS
               MOVE GRUPW01S-NUMERO-GRUPO-RET
                                          TO SLIG31-GRP-ECONOMICO-COMP
           ELSE
               MOVE ZEROS                 TO SLIG31-GRP-ECONOMICO-COMP
           END-IF.

           WRITE REG-VENDOR-COMPRADOR.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 114000-TESTAR-FS-COMPVDOR.

           ADD 1                       TO ACU-GRAVA-COMPVDOR.

      *---------------------------------------------------------------*
       320000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       321000-OBTER-DIR-REG            SECTION.
      *---------------------------------------------------------------*

           INITIALIZE                     COMU-DADOS-9014.
           MOVE ZEROS                  TO RETURN-CODE.

           MOVE  SLIG31-BCO-FORN       TO COMU-COD-EMPR.
           MOVE  SLIG31-AGENCIA-FORN   TO COMU-COD-DEPDC.
           MOVE 'VRS001'               TO COMU-VERSAO.
           MOVE 'MESU9014'             TO WRK-MODULO.

           CALL WRK-MODULO             USING COMU-DADOS-9014.

           IF  RETURN-CODE             NOT EQUAL ZEROS AND 08
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE  COMU-MENSAGEM     TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  RETURN-CODE             EQUAL ZEROS
               MOVE SLIG31-BCO-FORN    TO SLIG31-BCO-DIR-REGIONAL
               MOVE COMU-JUNC-DIR      TO SLIG31-DIR-REGIONAL
           ELSE
               MOVE ZEROS              TO SLIG31-BCO-DIR-REGIONAL
                                          SLIG31-DIR-REGIONAL
           END-IF.

      *---------------------------------------------------------------*
       321000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       322000-OBTER-GER-REG            SECTION.
      *---------------------------------------------------------------*

           INITIALIZE WRK-PARM.

           MOVE  17                     TO WRK-ENT-TPO-RELAC.
           MOVE 'SLIG6420'              TO WRK-ENT-NOME-PGM.
           MOVE 'B'                     TO WRK-AMBIENTE.

           MOVE  SLIG31-BCO-FORN        TO WRK-ENT-BANCO.
           MOVE  SLIG31-AGENCIA-FORN    TO WRK-ENT-AGENCIA.
           MOVE 'MESU9023'              TO WRK-MODULO.

           MOVE SPACES                  TO WRK-IO-PCB
                                           WRK-ALT-PCB
                                           WRK-ERRO-AREA
                                           WRK-SQLCA.

           CALL WRK-MODULO              USING WRK-AMBIENTE
                                              WRK-IO-PCB
                                              WRK-ALT-PCB
                                              WRK-PARM
                                              WRK-ERRO-AREA
                                              WRK-SQLCA.

           IF  WRK-ENT-COD-RETORNO      EQUAL 2
               MOVE 'APL'               TO ERR-TIPO-ACESSO
               MOVE WRK-ENT-COD-RETORNO TO WRK-COD-RET-9023
               MOVE WRK-ERRO-AREA       TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           ELSE
               IF  WRK-ENT-COD-RETORNO  EQUAL 88
                   MOVE 'DB2'           TO ERR-TIPO-ACESSO
                   MOVE 'MESU9023'      TO ERR-MODULO
                   PERFORM 999999-PROCESSAR-ROTINA-ERRO
               END-IF
           END-IF.

           IF  WRK-ENT-COD-RETORNO      EQUAL ZEROS
               MOVE WRK-SAI-EMPR-RELAC  TO SLIG31-BCO-GERC-REGIONAL
               MOVE WRK-SAI-DEPTO-RELAC TO SLIG31-GERC-REGIONAL
           ELSE
               MOVE ZEROS               TO SLIG31-BCO-GERC-REGIONAL
                                           SLIG31-GERC-REGIONAL
           END-IF.

      *---------------------------------------------------------------*
       322000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       323000-OBTER-GRUPO-ECONM        SECTION.
      *---------------------------------------------------------------*

           MOVE 'SLIG6420'             TO GRUPW01S-PGM-CHAMADOR.
           MOVE 'GRUP0930'             TO WRK-MODULO.
           MOVE SPACES                 TO WRK-IO-PCB
                                          WRK-ALT-PCB.

           CALL WRK-MODULO             USING GRUPW01S-REG-EMPRESA
                                             WRK-IO-PCB
                                             WRK-ALT-PCB.

           IF  GRUPW01S-RETURN-CODE  EQUAL 999
               MOVE GRUPW01S-ERRO-AREA TO ERRO-AREA
               MOVE 'GRUP0930'         TO ERR-MODULO
               MOVE GRUPW01S-SQLCA     TO WRK-SQLCA
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           ELSE
               IF  GRUPW01S-RETURN-CODE  NOT EQUAL ZEROS AND 1
                   MOVE 'APL'            TO ERR-TIPO-ACESSO
                   MOVE GRUPW01S-RETURN-CODE TO WRK-COD-RET-0930
                   MOVE WRK-ERRO-0930    TO ERR-TEXTO
               END-IF
           END-IF.

      *---------------------------------------------------------------*
       323000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       330000-FORMATAR-CAMPOS-BAXA     SECTION.
      *---------------------------------------------------------------*

           INITIALIZE REG-VENDOR-COMPRADOR.

           MOVE SLIG46-CGC-CPF-COMP-ST   OF BAXADECE
                                         TO SLIG31-CGC-CPF-COMP-ST.
           MOVE SLIG46-FILIAL-COMP-ST    OF BAXADECE
                                         TO SLIG31-FILIAL-COMP-ST.
           MOVE SLIG46-CONTROLE-COMP-ST  OF BAXADECE
                                         TO SLIG31-CONTROLE-COMP-ST.
           MOVE SLIG46-TIPO-PESSOA-COMP  OF BAXADECE
                                         TO SLIG31-TIPO-PESSOA-COMP.
           MOVE SLIG46-BCO-COMP          OF BAXADECE
                                         TO SLIG31-BCO-COMP.
           MOVE SLIG46-AGENCIA-COMP      OF BAXADECE
                                         TO SLIG31-AGENCIA-COMP.
           MOVE SLIG46-CONTA-COMP        OF BAXADECE
                                         TO SLIG31-CONTA-COMP.
           MOVE SLIG46-NOME-COMP         OF BAXADECE
                                         TO SLIG31-NOME-COMP.
           MOVE SLIG46-CPRODT            OF BAXADECE
                                         TO SLIG31-CPRODT.
           MOVE SLIG46-CFAML OF BAXADECE TO SLIG31-CFAML.
           MOVE SLIG46-CCONTR            OF BAXADECE
                                         TO SLIG31-CCONTR.
           MOVE SLIG46-CSEQ  OF BAXADECE TO SLIG31-CSEQ.
           MOVE SLIG46-CPARCELA          OF BAXADECE
                                         TO SLIG31-CPARCELA.
           MOVE SLIG46-NUM-PAGTO         OF BAXADECE
                                         TO SLIG31-NUM-PAGTO.
           MOVE SLIG46-NOSSO-NUMERO      OF BAXADECE
                                         TO SLIG31-NOSSO-NUMERO.
           MOVE SLIG46-CGC-CPF-FORN-ST   OF BAXADECE
                                         TO SLIG31-CGC-CPF-FORN-ST.
           MOVE SLIG46-FILIAL-FORN-ST    OF BAXADECE
                                         TO SLIG31-FILIAL-FORN-ST.
           MOVE SLIG46-CONTROLE-FORN-ST  OF BAXADECE
                                         TO SLIG31-CONTROLE-FORN-ST.
           MOVE SLIG46-TIPO-PESSOA-FORN  OF BAXADECE
                                         TO SLIG31-TIPO-PESSOA-FORN.
           MOVE SLIG46-BCO-FORN          OF BAXADECE
                                         TO SLIG31-BCO-FORN.
           MOVE SLIG46-AGENCIA-FORN      OF BAXADECE
                                         TO SLIG31-AGENCIA-FORN.
           MOVE SLIG46-CONTA-FORN        OF BAXADECE
                                         TO SLIG31-CONTA-FORN.
           MOVE SLIG46-NOME-FORN         OF BAXADECE
                                         TO SLIG31-NOME-FORN.
           MOVE SLIG46-DT-VENCTO-PARC    OF BAXADECE
                                         TO SLIG31-DT-VENCTO-PARC.
           MOVE SLIG46-DT-PAGTO-PARC     OF BAXADECE
                                         TO SLIG31-DT-PAGTO-PARC.
           MOVE SLIG46-CTPO-SIT-PARC     OF BAXADECE
                                         TO SLIG31-CPTO-SIT-PARC.
           MOVE SLIG46-DT-ENTRADA-ORIGEM OF BAXADECE
                                         TO SLIG31-DT-ENTRADA-ORIGEM.
           MOVE SLIG46-VL-PRINC-PARC     OF BAXADECE
                                         TO SLIG31-VL-PRINC-PARC.
           MOVE SLIG46-IND-PAGTO         OF BAXADECE
                                         TO SLIG31-IND-PAGTO.

      *---------------------------------------------------------------*
       330000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       340000-FORMATAR-CAMPOS-FINAV    SECTION.
      *---------------------------------------------------------------*

           INITIALIZE REG-VENDOR-COMPRADOR.

           MOVE SLIG64-CGC-CPF-COMP-ST   TO SLIG31-CGC-CPF-COMP-ST.
           MOVE SLIG64-FILIAL-COMP-ST    TO SLIG31-FILIAL-COMP-ST.
           MOVE SLIG64-CONTROLE-COMP-ST  TO SLIG31-CONTROLE-COMP-ST.
           MOVE SLIG64-TIPO-PESSOA-COMP  TO SLIG31-TIPO-PESSOA-COMP.
           MOVE SLIG64-BCO-COMP          TO SLIG31-BCO-COMP.
           MOVE SLIG64-AGENCIA-COMP      TO SLIG31-AGENCIA-COMP.
           MOVE SLIG64-CONTA-COMP        TO SLIG31-CONTA-COMP.
           MOVE SLIG64-NOME-COMP         TO SLIG31-NOME-COMP.
           MOVE SLIG64-CPRODT            TO SLIG31-CPRODT.
           MOVE SLIG64-CFAML             TO SLIG31-CFAML.
           MOVE SLIG64-CCONTR            TO SLIG31-CCONTR.
           MOVE SLIG64-CSEQ              TO SLIG31-CSEQ.
           MOVE SLIG64-CPARCELA          TO SLIG31-CPARCELA.
           MOVE SLIG64-NOSSO-NUMERO      TO SLIG31-NOSSO-NUMERO.
           MOVE SLIG64-CGC-CPF-FORN-ST      TO SLIG31-CGC-CPF-FORN-ST.
           MOVE SLIG64-FILIAL-FORN-ST       TO SLIG31-FILIAL-FORN-ST.
           MOVE SLIG64-CONTROLE-FORN-ST     TO SLIG31-CONTROLE-FORN-ST.
           MOVE SLIG64-TIPO-PESSOA-FORN  TO SLIG31-TIPO-PESSOA-FORN.
           MOVE SLIG64-BCO-FORN          TO SLIG31-BCO-FORN.
           MOVE SLIG64-AGENCIA-FORN      TO SLIG31-AGENCIA-FORN.
           MOVE SLIG64-CONTA-FORN        TO SLIG31-CONTA-FORN.
           MOVE SLIG64-NOME-FORN         TO SLIG31-NOME-FORN.
           MOVE SLIG64-DT-VENCTO-PARC    TO SLIG31-DT-VENCTO-PARC.
           MOVE SLIG64-CTPO-SIT-PARC     TO SLIG31-CPTO-SIT-PARC.
           MOVE SLIG64-DT-ENTRADA-ORIGEM TO SLIG31-DT-ENTRADA-ORIGEM.
           MOVE SLIG64-VL-PRINC-PARC     TO SLIG31-VL-PRINC-PARC.
           MOVE '0'                      TO SLIG31-IND-PAGTO.

      *---------------------------------------------------------------*
       340000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       400000-FINALIZAR                SECTION.
      *---------------------------------------------------------------*

           PERFORM 410000-EMITIR-SRELCTRL.

           CLOSE PGTODECE
                 BAXADECE
                 FINVNDOR
                 COMPVDOR
                 SRELCTRL.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 110000-TESTAR-FILE-STATUS.

           GOBACK.

      *---------------------------------------------------------------*
       400000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       410000-EMITIR-SRELCTRL          SECTION.
      *---------------------------------------------------------------*

           CALL 'POOL7600'             USING WRK-AREA-POOL7600.

           MOVE WRK-DIA-7600           TO CB2-DIA.
           MOVE WRK-MES-7600           TO CB2-MES.
           MOVE WRK-ANO-7600           TO CB2-ANO.
           MOVE ACU-LIDOS-PGTODECE     TO LT1-TOT-LIDOS-PGTODECE.
           MOVE ACU-LIDOS-BAXADECE     TO LT2-TOT-LIDOS-BAXADECE.
           MOVE ACU-LIDOS-FINVNDOR     TO LT3-TOT-LIDOS-FINVNDOR.
           MOVE ACU-GRAVA-COMPVDOR     TO LT4-TOT-GRAVA-COMPVDOR.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE REG-SRELCTRL          FROM CABEC1.

           PERFORM 115000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM CABEC2.

           PERFORM 115000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM CABEC3.

           PERFORM 115000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM CABEC4.

           PERFORM 115000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT1.

           PERFORM 115000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT2.

           PERFORM 115000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT3.

           PERFORM 115000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT4.

           PERFORM 115000-TESTAR-FS-SRELCTRL.

      *---------------------------------------------------------------*
       410000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       999999-PROCESSAR-ROTINA-ERRO    SECTION.
      *---------------------------------------------------------------*

           MOVE 'SLIG6420'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             WRK-SQLCA.

           GOBACK.

      *---------------------------------------------------------------*
       999999-99-FIM.                  EXIT.
      *---------------------------------------------------------------*
