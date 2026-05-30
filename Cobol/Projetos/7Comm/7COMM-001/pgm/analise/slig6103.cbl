      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. SLIG6103.
       AUTHOR.     REGINALDO PEDROSO.
      *================================================================*
      *                    D T S  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   SLIG6103                                    *
      *    PROGRAMADOR.:   REGINALDO PEDROSO       - DTS               *
      *    ANALISTA....:   REGINALDO PEDROSO       - DTS               *
      *    DATA........:   24/08/2004                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   PESQUISA GRUPO DO SACADO A PARTIR DO CGC DO *
      *      SACADO NA ROTINA GRUP0930.                                *
      *                                                                *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                    CLISACIN                  I#SLIG02          *
      *                    CLISACOU                  I#SLIG02          *
      *                    SRELCTRL                     -              *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERRO                     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD7100 - MODULO DE TRATAMENTO DE ERRO                     *
      *================================================================*
      *================================================================*
      *        ULTIMA ALTERACAO   31/01/2005                           *
      *----------------------------------------------------------------*
      *                                                                *
      *        ANALISTA   : IVAO ANDERSON AKIOKA - JAPI                *
      *                                                                *
      *        DATA       : JANEIRO/2005                               *
      *                                                                *
      *        OBJETIVO   : MELHORIA DE PERFORMANCE                    *
      *                                                                *
      *        - GRAVACAO DO ARQUIVO CLISACOU NO FORMATO COMPRIMIDO    *
      *        - BUFERIZACAO DOS ARQUIVOS                              *
      *        - RETIRADA COMANDO INITIALIZE AO CHAMAR MODULO MESU9014 *
      *        - CONVERTER O PROGRAMA PARA COBOL FOR MVS               *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT CLISACIN ASSIGN      TO UT-S-CLISACIN
           FILE STATUS                 IS WRK-FS-CLISACIN.

           SELECT SRELCTRL ASSIGN      TO UT-S-SRELCTRL
           FILE STATUS                 IS WRK-FS-SRELCTRL.

           EJECT
      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE CLIENTES CLASSIFICADO PELO SACADO        *
      *            ORG. SEQUENCIAL     -   LRECL = 250                 *
      *----------------------------------------------------------------*

       FD  CLISACIN
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

-INC I#SLIG02

           EJECT
      *----------------------------------------------------------------*
      *    OUTPUT: CONTROLE DE PROCESSAMENTO                           *
      *            ORG. SEQUENCIAL     -   LRECL   =   080             *
      *----------------------------------------------------------------*

       FD  SRELCTRL
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-SRELCTRL                PIC  X(080).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** SLIG6103 - INICIO DA AREA DE WORKING ***'.

      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CLIENTES COM GRUPO SACADO ATUALIZADO     *
      *            ORG. SEQUENCIAL     -   LRECL   =   250             *
      *----------------------------------------------------------------*

CNPJAN 01  REG-CLISACOU                PIC  X(300)         VALUE SPACES.


      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** ACUMULADORES ***'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-CLISACIN          PIC  9(013) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-CLISACOU          PIC  9(013) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE TESTES DE FILE STATUS ***'.
      *----------------------------------------------------------------*

       77  WRK-FS-CLISACIN             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SRELCTRL             PIC  X(002)         VALUE SPACES.

       77  WRK-OPERACAO                PIC  X(013)         VALUE SPACES.
       77  WRK-ABERTURA                PIC  X(013)         VALUE
           'NA  ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-CLISACOU                PIC  X(008)         VALUE
           'CLISACOU'.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-MODULO                  PIC  X(008)     VALUE 'GRUP201C'.
       77  WRK-SQLCA                   PIC  X(136)         VALUE SPACES.
       77  WRK-ABEND                   PIC S9(004) COMP    VALUE +0001.
       77  WRK-RETURN-CODE             PIC  9(002)         VALUE ZEROS.
       77  WRK-FIM-CLISACIN            PIC  X(001)         VALUE SPACES.

       77  WRK-NRO-GRUPO-SACADO        PIC  9(009)         VALUE ZEROS.
CNPJ7C 77  WRK-NRO-GRUPO-SACADO-X  REDEFINES
CNPJ7C     WRK-NRO-GRUPO-SACADO        PIC  X(009).
       77  WRK-TOT-GRUP-PESQ           PIC  9(015) COMP-3  VALUE ZEROS.
       77  WRK-TOT-GRUP-ACHOU          PIC  9(015) COMP-3  VALUE ZEROS.
       77  WRK-TOT-GRUP-NAO-ACHOU      PIC  9(015) COMP-3  VALUE ZEROS.

       77  WRK-MENSA01                 PIC  X(075)         VALUE
           '** ARQUIVO CLISACIN VAZIO **'.

       01  WRK-CGC-CPF-ANT.
           03  WRK-CGC-CPF-SAC-ANT     PIC  X(009)         VALUE ZEROS.
CNPJAN     03  WRK-FILIAL-SAC-ANT      PIC  X(005)         VALUE ZEROS.
           03  WRK-CONTROLE-SAC-ANT    PIC  9(002)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO RELATORIO SRELCTRL ***'.
      *----------------------------------------------------------------*

       01  CABEC1.
           03  FILLER                  PIC  X(001)         VALUE '1'.
           03  FILLER                  PIC  X(016)         VALUE
               'BRADESCO'.
           03  FILLER                  PIC  X(053)         VALUE
               'SLIG - SISTEMA DE INDICE DE LIQUIDEZ'.
           03  FILLER                  PIC  X(010)         VALUE
               'FOL. UNICA'.

       01  CABEC2.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(069)         VALUE
               'SLIG6103'.
      *    03  CB2-DIA                 PIC  9(002)         VALUE ZEROS.
      *    03  FILLER                  PIC  X(001)         VALUE '/'.
      *    03  CB2-MES                 PIC  9(002)         VALUE ZEROS.
      *    03  FILLER                  PIC  X(001)         VALUE '/'.
      *    03  CB2-ANO                 PIC  9(004)         VALUE ZEROS.

       01  CABEC3.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(015)         VALUE SPACES.
           03  FILLER                  PIC  X(064)         VALUE
               'ATUALIZA  CNPJ  DO  GRUPO  DO  SACADO '.

       01  CABEC4.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(025)         VALUE SPACES.
           03  FILLER                  PIC  X(054)         VALUE
               'CONTROLE OPERACIONAL'.

       01  LINTOT1.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(035)         VALUE
               'REGISTROS LIDOS       (CLISACIN) : '.
           03  LT1-LIDOS-CLISACIN      PIC  ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT2.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(035)         VALUE
               'REGISTROS GRAVADOS    (CLISACOU) : '.
           03  LT2-GRAVA-CLISACOU      PIC  ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT3.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(035)         VALUE
               'REGISTROS PESQUISADOS (GRUP0930) : '.
           03  LT3-GRUP-PESQ           PIC  ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT4.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(035)         VALUE
               'REGS SAC ENCONTRADOS  (GRUP0930) : '.
           03  LT4-GRUP-ACHOU          PIC  ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT5.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(035)         VALUE
               'REGS SAC NAO ENCONTRAD(GRUP0930) : '.
           03  LT5-GRUP-NAO-ACHOU      PIC  ZZZ.ZZZ.ZZZ.ZZ9.

           EJECT

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO MODULO GRUP0930 ***'.
      *----------------------------------------------------------------*

       01  COMU-AMBIENTE               PIC  X(001)         VALUE SPACES.
       01  COMU-IO-PCB                 PIC  X(032)         VALUE SPACES.
       01  COMU-ALT-PCB                PIC  X(032)         VALUE SPACES.
       01  COMU-ERRO-AREA              PIC  X(107)         VALUE SPACES.

       01  COMU-PARM.
           03  COMU-ENTRADA.
               05  COMU-NOME-PGM       PIC  X(008)         VALUE SPACES.
               05  COMU-COD-RETORNO    PIC  9(002)         VALUE ZEROS.
               05  COMU-TPO-RELAC      PIC  9(003)         VALUE ZEROS.
               05  COMU-EMPRESA        PIC  9(005)         VALUE ZEROS.
               05  COMU-DEPDC          PIC  9(005)         VALUE ZEROS.
           03  COMU-SAIDA.
               05  COMU-EMPR-RELAC     PIC  9(005)         VALUE ZEROS.
               05  COMU-DEPTO-RELAC    PIC  9(005)         VALUE ZEROS.
               05  COMU-NOME-DEPDC     PIC  X(050)         VALUE SPACES.

       COPY GRUPW01S.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA POOL7100 ***'.
      *----------------------------------------------------------------*

-INC POL7100C

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** SLIG6103 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
           EJECT
      *================================================================*
       PROCEDURE DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       000000-INICIAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 100000-INICIALIZAR.

           PERFORM 200000-VERIFICAR-VAZIO.

           PERFORM 300000-PROCESSAR    UNTIL
                   WRK-FIM-CLISACIN    EQUAL 'S'.

           MOVE ZEROS TO RETURN-CODE

           PERFORM 400000-FINALIZAR.

      *----------------------------------------------------------------*
       000000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  CLISACIN
                OUTPUT SRELCTRL.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 110000-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       110000-TESTAR-FILE-STATUS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 111000-TESTAR-FS-CLISACIN.

           PERFORM 112000-TESTAR-FS-SRELCTRL.

      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       111000-TESTAR-FS-CLISACIN       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CLISACIN         NOT EQUAL '00'
               DISPLAY '****************** SLIG6103 ******************'
               DISPLAY '*                                            *'
               DISPLAY '*        ERRO ' WRK-OPERACAO
                                                  ' DO ARQUIVO       *'
               DISPLAY '*                  CLISACIN                  *'
               DISPLAY '*              FILE STATUS = ' WRK-FS-CLISACIN
                                                      '              *'
               DISPLAY '*                                            *'
               DISPLAY '****************** SLIG6103 ******************'
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       111000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       112000-TESTAR-FS-SRELCTRL       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SRELCTRL         NOT EQUAL ZEROS
               DISPLAY '****************** SLIG6103 ******************'
               DISPLAY '*                                            *'
               DISPLAY '*        ERRO ' WRK-OPERACAO
                                                  ' DO ARQUIVO       *'
               DISPLAY '*                  SRELCTRL                  *'
               DISPLAY '*              FILE STATUS = ' WRK-FS-SRELCTRL
                                                      '              *'
               DISPLAY '*                                            *'
               DISPLAY '****************** SLIG6103 ******************'
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       112000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       200000-VERIFICAR-VAZIO          SECTION.
      *----------------------------------------------------------------*

           PERFORM 210000-LER-CLISACIN.

           IF  WRK-FIM-CLISACIN         EQUAL 'S'
               DISPLAY '****************** SLIG6103 ******************'
               DISPLAY '*                                            *'
               DISPLAY '*          ARQUIVO CLISACIN VAZIO            *'
               DISPLAY '*                                            *'
               DISPLAY '*         PROCESSAMENTO  ENCERRADO           *'
               DISPLAY '*                                            *'
               DISPLAY '****************** SLIG6103 ******************'
               MOVE 'APL'               TO ERR-TIPO-ACESSO
               MOVE WRK-MENSA01         TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       210000-LER-CLISACIN             SECTION.
      *----------------------------------------------------------------*

           READ CLISACIN.

           IF  WRK-FS-CLISACIN         EQUAL '10'
               MOVE 'S'                TO    WRK-FIM-CLISACIN
               GO TO 210000-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 111000-TESTAR-FS-CLISACIN.

           ADD 1                       TO ACU-LIDOS-CLISACIN.

      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       300000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           IF  SLIG02-CGC-CPF-SACADO-ST
           NOT EQUAL ZEROS
               PERFORM 320000-OBTER-GRUPO-SACADO
           END-IF.

           PERFORM 310000-GRAVAR-CLISACOU.

           PERFORM 210000-LER-CLISACIN.

      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       310000-GRAVAR-CLISACOU          SECTION.
      *----------------------------------------------------------------*

           MOVE REG-INTERMEDIARIO-TITULO TO REG-CLISACOU
           CALL 'BRAD0315' USING WRK-CLISACOU REG-CLISACOU.

           ADD 1                       TO ACU-GRAVA-CLISACOU.

      *----------------------------------------------------------------*
       310000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       320000-OBTER-GRUPO-SACADO       SECTION.
      *----------------------------------------------------------------*

           IF (SLIG02-CGC-CPF-SACADO-ST
           NOT =  WRK-CGC-CPF-SAC-ANT)  OR
              (SLIG02-FILIAL-SACADO-ST
              NOT =  WRK-FILIAL-SAC-ANT)   OR
              (SLIG02-CONTROLE-SACADO-ST
              NOT =  WRK-CONTROLE-SAC-ANT)

              MOVE ZEROS                   TO GRUPW01S-NUMERO-GRUPO-ENV
              MOVE ZEROS                     TO GRUPW01S-CGC-CPF(1:2)
              MOVE SLIG02-CGC-CPF-SACADO-ST  TO GRUPW01S-CGC-CPF(3:9)
              MOVE SLIG02-FILIAL-SACADO-ST(2:)
                                             TO GRUPW01S-FILIAL-CGC-CPF
              MOVE SLIG02-CONTROLE-SACADO-ST TO GRUPW01S-CTRL-CGC-CPF

              PERFORM 330000-ACESSAR-MODULO-GRUP0930

              MOVE SLIG02-CGC-CPF-SACADO-ST
              TO WRK-CGC-CPF-SAC-ANT
              MOVE SLIG02-FILIAL-SACADO-ST
              TO WRK-FILIAL-SAC-ANT
              MOVE SLIG02-CONTROLE-SACADO-ST
              TO WRK-CONTROLE-SAC-ANT
           END-IF.

           MOVE WRK-NRO-GRUPO-SACADO-X TO SLIG02-CGC-GRUPO-SACADO-ST.
           MOVE WRK-NRO-GRUPO-SACADO   TO SLIG02-CGC-GRUPO-SACADO.

      *----------------------------------------------------------------*
       320000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       330000-ACESSAR-MODULO-GRUP0930  SECTION.
      *----------------------------------------------------------------*

           MOVE 'SLIG6103'                 TO GRUPW01S-PGM-CHAMADOR.
           MOVE  SPACES                    TO COMU-IO-PCB
                                              COMU-ALT-PCB.

           CALL WRK-MODULO USING GRUPW01S-REG-EMPRESA
                                                 COMU-IO-PCB
                                                 COMU-ALT-PCB.

           ADD  1                          TO WRK-TOT-GRUP-PESQ.

           IF  GRUPW01S-RETURN-CODE        NOT EQUAL ZEROS
               MOVE ZEROS                  TO GRUPW01S-NUMERO-GRUPO-RET
               ADD  1                      TO WRK-TOT-GRUP-NAO-ACHOU
           ELSE
               ADD  1                      TO WRK-TOT-GRUP-ACHOU
           END-IF.

           MOVE GRUPW01S-NUMERO-GRUPO-RET  TO WRK-NRO-GRUPO-SACADO.

      *----------------------------------------------------------------*
       330000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       400000-FINALIZAR                SECTION.
      *----------------------------------------------------------------*

      *    IF  ACU-LIDOS-CLISACIN      GREATER ZEROS
      *        PERFORM 310000-GRAVAR-CLISACOU
      *    END-IF.

           PERFORM 410000-IMPRIMIR-SRELCTRL.

           CLOSE CLISACIN
                 SRELCTRL.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           IF  ACU-GRAVA-CLISACOU GREATER ZEROS
               CALL 'FECHAARQ' USING WRK-CLISACOU.

           PERFORM 110000-TESTAR-FILE-STATUS.

           MOVE ZEROS TO RETURN-CODE

           GOBACK.

      *----------------------------------------------------------------*
       400000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       410000-IMPRIMIR-SRELCTRL        SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS-CLISACIN     TO LT1-LIDOS-CLISACIN.
           MOVE ACU-GRAVA-CLISACOU     TO LT2-GRAVA-CLISACOU.
           MOVE WRK-TOT-GRUP-PESQ      TO LT3-GRUP-PESQ.
           MOVE WRK-TOT-GRUP-ACHOU     TO LT4-GRUP-ACHOU.
           MOVE WRK-TOT-GRUP-NAO-ACHOU TO LT5-GRUP-NAO-ACHOU.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE REG-SRELCTRL          FROM CABEC1.

           PERFORM 112000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM CABEC2.

           PERFORM 112000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM CABEC3.

           PERFORM 112000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM CABEC4.

           PERFORM 112000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT1.

           PERFORM 112000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT2.

           PERFORM 112000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT3.

           PERFORM 112000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT4.

           PERFORM 112000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT5.

           PERFORM 112000-TESTAR-FS-SRELCTRL.

      *----------------------------------------------------------------*
       410000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       999999-PROCESSAR-ROTINA-ERRO    SECTION.
      *----------------------------------------------------------------*

           MOVE 'SLIG6103'             TO ERR-PGM

           CALL 'BRAD7100' USING WRK-BATCH
                                             ERRO-AREA
                                             WRK-SQLCA.

           GOBACK.
