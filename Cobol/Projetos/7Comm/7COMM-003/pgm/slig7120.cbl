      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. SLIG7120.
       AUTHOR.     CAMILA.
      *================================================================*
      *                    D T S  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   SLIG7120                                    *
      *    PROGRAMADOR.:   CAMILA   -  DTS CONSULTING                  *
      *    ANALISTA....:   CAMILA   -  DTS CONSULTING                  *
      *    DATA........:   25/03/2005                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR ARQUIVO DE EMITENTES SELECIONADOS -   *
      *      PRODUTO CHEQUES.                                          *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                        INCLUDE/BOOK      *
      *                    ARQDATAS                     I#SLIG00       *
      *                    MOVTOCHQ                     I#SLIG74       *
      *                    MVSELCHQ                     I#SLIG62       *
      *                    SRELCTRL                       -            *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERRO PELA POOL7100       *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL7100 - TRATAMENTO DE ERROS                              *
      *    POOL7600 - OBTER DATA E HORA DO SISTEMA                     *
      *    BRAD0300 - LEITURA  DO ARQUIVO COMPRIMIDO                   *
      *================================================================*
           EJECT
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

           SELECT ARQDATAS ASSIGN      TO UT-S-ARQDATAS
           FILE STATUS                 IS WRK-FS-ARQDATAS.

           SELECT MVSELCHQ ASSIGN      TO UT-S-MVSELCHQ
           FILE STATUS                 IS WRK-FS-MVSELCHQ.

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
      *    INPUT   ARQUIVO DE DATAS                                    *
      *            ORG. SEQUENCIAL     -   LRECL   =   100             *
      *----------------------------------------------------------------*

       FD  ARQDATAS
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

-INC I#SLIG00

           EJECT
      *----------------------------------------------------------------*
      *    OUTPUT  ARQ. REGS. SELECIONADOS CHEQUES                     *
      *            ORG. SEQUENCIAL     -   LRECL   =   308             *
      *----------------------------------------------------------------*

       FD  MVSELCHQ
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

-INC I#SLIG62

           EJECT
      *----------------------------------------------------------------*
      *    OUTPUT: RELATORIO DE CONTROLE                               *
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
           '*** SLIG7120 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACUMULADORES ***'.
      *----------------------------------------------------------------*

       77  ACU-DESPREZADOS             PIC  9(013) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-MOVTOCHQ          PIC  9(013) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-MVSELCHQ          PIC  9(013) COMP-3  VALUE ZEROS.
       77  ACU-EMITT-DESPR             PIC  9(013) COMP-3  VALUE ZEROS.

           EJECT
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA TESTES DE FILE STATUS ***'.
      *----------------------------------------------------------------*

       77  WRK-FS-ARQDATAS             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-MOVTOCHQ             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-MVSELCHQ             PIC  X(002)         VALUE SPACES.
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
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-MSG01                   PIC  X(075)         VALUE
           '** ARQUIVO MOVTOCHQ VAZIO **'.
       77  WRK-MSG03                   PIC  X(075)         VALUE
           '** ARQUIVO MOVTOCHQ FORA DE SEQUENCIA **'.
       77  WRK-MSG04                   PIC  X(075)         VALUE
           '** NENHUM EMITT / TP-PESSOA FOI ENCONTRADO **'.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-MOVTOCHQ                PIC  X(008)         VALUE
           'MOVTOCHQ'.

       01  WRK-PERIODO-PROCESS.
           03  WRK-DT-PER-INI          PIC  9(008)         VALUE ZEROS.
           03  WRK-DT-PER-FIM          PIC  9(008)         VALUE ZEROS.

       01  WRK-CHV-MOVTOCHQ.
           03  WRK-CPF-MOVTOCHQ        PIC  X(009)         VALUE SPACES.
           03  WRK-TPS-MOVTOCHQ        PIC  X(001)         VALUE SPACES.

       01  WRK-CHV-MOVTOCHQ-ANT.
           03  WRK-CPF-MOVTOCHQ-ANT    PIC  X(009)         VALUE SPACES.
           03  WRK-TPS-MOVTOCHQ-ANT    PIC  X(001)         VALUE SPACES.

       01  WRK-CNEGOC                  PIC  9(011)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-CNEGOC.
           03  WRK-AGENCIA             PIC  9(004).
           03  WRK-CONTA               PIC  9(007).

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA MENSAGEM DE ERRO FILE STATUS ***'.
      *----------------------------------------------------------------*

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

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA POOL7600 ***'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           03  WRK-DATA-JULIANA        PIC  9(005) COMP-3  VALUE ZEROS.
           03  WRK-DATA-AAMMDD         PIC  9(007) COMP-3  VALUE ZEROS.
           03  WRK-DATA-AAAAMMDD       PIC  9(009) COMP-3  VALUE ZEROS.
           03  WRK-HORA-HHMMSS         PIC  9(007) COMP-3  VALUE ZEROS.
           03  WRK-HORA-HHMMSSMMMMMM   PIC  9(013) COMP-3  VALUE ZEROS.
           03  WRK-TIMESTAMP.
               05  WRK-ANO-SIS         PIC  9(004)         VALUE ZEROS.
               05  WRK-MES-SIS         PIC  9(002)         VALUE ZEROS.
               05  WRK-DIA-SIS         PIC  9(002)         VALUE ZEROS.
               05  FILLER              PIC  X(012)         VALUE SPACES.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** MENSAGENS DE ERRO - BRAD0300 ***'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-TEXTO.
           03  FILLER                  PIC  X(011)         VALUE
               '** ERRO NA '.
           03  WRK-NOME-POOL           PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(022)         VALUE
               ' NO ACESSO AO ARQUIVO '.
           03  WRK-NOME-ARQ            PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(017)         VALUE
               ' - RETURN CODE = '.
           03  WRK-RETURN-CODE         PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(003)         VALUE ' **'.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA LAYOUT DO RELATORIO ***'.
      *----------------------------------------------------------------*

       01  CABEC1.
           03  FILLER                  PIC  X(001)         VALUE '1'.
           03  FILLER                  PIC  X(020)         VALUE
               'BRADESCO'.
           03  FILLER                  PIC  X(049)         VALUE
               'SISTEMA DE INDICE DE LIQUIDEZ'.
           03  FILLER                  PIC  X(010)         VALUE
               'PAG. UNICA'.

       01  CABEC2.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(069)         VALUE
               'SLIG7120'.
           03  CB2-DIA                 PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB2-MES                 PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB2-ANO                 PIC  9(004)         VALUE ZEROS.

       01  CABEC3.
           03  FILLER                  PIC  X(001)         VALUE '-'.
           03  FILLER                  PIC  X(026)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'CONTROLE OPERACIONAL'.

       01  LINTOT2.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(014)         VALUE
               '    TOTAL DE '.
           03  FILLER                  PIC  X(040)         VALUE
               'REGISTROS LIDOS    (MOVTOCHQ) . . . . : '.
           03  LT2-LIDOS               PIC  ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT3.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(014)         VALUE
               '    TOTAL DE '.
           03  FILLER                  PIC  X(040)         VALUE
               'REGISTROS GRAVADOS (MVSELCHQ) . . . . : '.
           03  LT3-GRAVADOS            PIC  ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT4.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(014)         VALUE
               '    TOTAL DE '.
           03  FILLER                  PIC  X(040)         VALUE
               'REGISTROS DESPREZADOS . . . . . . . . : '.
           03  LT4-N-ENCONT            PIC  ZZZ.ZZZ.ZZZ.ZZ9.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** ARQUIVO CHEQUES PARA RELATORIO        ***'.
      *----------------------------------------------------------------*

-INC I#SLIG74

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA POOL7100 ***'.
      *----------------------------------------------------------------*

-INC POL7100C

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(075)         VALUE
           '*** SLIG7120 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
           EJECT
      *-----------------------------------------------------------------
       LINKAGE SECTION.
      *-----------------------------------------------------------------
       01  LNK-PARM.
           03  LNK-LEN                 PIC 9(04)  COMP.
           03  LNK-PARM-PROC.
               05  LNK-PARM-CHV1.
                   07  LNK-PARM-SAC1       PIC X(09).
                   07  LNK-PARM-TPS1       PIC X(01).
               05  LNK-BRANCOS1        PIC X(01).
               05  LNK-PARM-CHV2.
                   07  LNK-PARM-SAC2       PIC X(09).
                   07  LNK-PARM-TPS2       PIC X(01).
               05  LNK-BRANCOS2        PIC X(01).
               05  LNK-PARM-CHV3.
                   07  LNK-PARM-SAC3       PIC X(09).
                   07  LNK-PARM-TPS3       PIC X(01).
               05  LNK-BRANCOS3        PIC X(01).
               05  LNK-PARM-CHV4.
                   07  LNK-PARM-SAC4       PIC X(09).
                   07  LNK-PARM-TPS4       PIC X(01).
               05  LNK-BRANCOS4        PIC X(01).
               05  LNK-PARM-CHV5.
                   07  LNK-PARM-SAC5       PIC X(09).
                   07  LNK-PARM-TPS5       PIC X(01).

      *================================================================*
       PROCEDURE                       DIVISION USING LNK-PARM.
      *================================================================*

      *----------------------------------------------------------------*
       000000-INICIAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 100000-INICIALIZAR.

           PERFORM 12000-VERIFICAR-PARAMETRO

           PERFORM 200000-VERIFICAR-VAZIOS.

           PERFORM 300000-PROCESSAR   UNTIL
                   WRK-FS-MOVTOCHQ    EQUAL 'S'.

           PERFORM 400000-FINALIZAR.

      *----------------------------------------------------------------*
       000000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  ARQDATAS
                OUTPUT MVSELCHQ
                       SRELCTRL.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 110000-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       110000-TESTAR-FILE-STATUS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 112000-TESTAR-FS-MVSELCHQ.
           PERFORM 113000-TESTAR-FS-SRELCTRL.
           PERFORM 114000-TESTAR-FS-ARQDATAS.

      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       112000-TESTAR-FS-MVSELCHQ       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-MVSELCHQ         NOT EQUAL ZEROS
               MOVE 'MVSELCHQ'         TO WRK-NOME-ARQUIVO
               MOVE WRK-FS-MVSELCHQ    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       112000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       113000-TESTAR-FS-SRELCTRL       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SRELCTRL         NOT EQUAL '00'
               MOVE 'SRELCTRL'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-SRELCTRL   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       113000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       114000-TESTAR-FS-ARQDATAS       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQDATAS         NOT EQUAL '00'
               MOVE 'ARQDATAS'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-ARQDATAS   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       114000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       12000-VERIFICAR-PARAMETRO       SECTION.
      *----------------------------------------------------------------*
           IF (LNK-PARM-SAC1           NOT NUMERIC)   OR
              (LNK-PARM-SAC2           NOT NUMERIC)   OR
              (LNK-PARM-SAC3           NOT NUMERIC)   OR
              (LNK-PARM-SAC4           NOT NUMERIC)   OR
              (LNK-PARM-SAC5           NOT NUMERIC)   OR
              (LNK-PARM-TPS1           NOT EQUAL 'F' AND
               LNK-PARM-TPS1           NOT EQUAL 'J') OR
              (LNK-PARM-TPS2           NOT EQUAL 'F' AND
               LNK-PARM-TPS2           NOT EQUAL 'J') OR
              (LNK-PARM-TPS3           NOT EQUAL 'F' AND
               LNK-PARM-TPS3           NOT EQUAL 'J') OR
              (LNK-PARM-TPS4           NOT EQUAL 'F' AND
               LNK-PARM-TPS4           NOT EQUAL 'J') OR
              (LNK-PARM-TPS5           NOT EQUAL 'F' AND
               LNK-PARM-TPS5           NOT EQUAL 'J')
              MOVE 'PARM INVALIDO'     TO ERR-TEXTO
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              DISPLAY '*----------------------------------------*'
              DISPLAY '*   PARM INVALIDO = ' LNK-PARM-PROC
              DISPLAY '*----------------------------------------*'
              PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF (LNK-PARM-SAC1           EQUAL '000000000') AND
              (LNK-PARM-SAC2           EQUAL '000000000') AND
              (LNK-PARM-SAC3           EQUAL '000000000') AND
              (LNK-PARM-SAC4           EQUAL '000000000') AND
              (LNK-PARM-SAC5           EQUAL '000000000') AND
              (LNK-PARM-TPS1           EQUAL 'X')         AND
              (LNK-PARM-TPS2           EQUAL 'X')         AND
              (LNK-PARM-TPS3           EQUAL 'X')         AND
              (LNK-PARM-TPS4           EQUAL 'X')         AND
              (LNK-PARM-TPS5           EQUAL 'X')
              MOVE 'PARM INVALIDO'     TO ERR-TEXTO
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              DISPLAY '*----------------------------------------*'
              DISPLAY '*   PARM INVALIDO = ' LNK-PARM-PROC
              DISPLAY '*----------------------------------------*'
              PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       12000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       200000-VERIFICAR-VAZIOS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 210000-LER-ARQDATAS.

           IF  WRK-FS-ARQDATAS         EQUAL '10' AND
               ACU-DESPREZADOS         EQUAL ZEROS
               DISPLAY '***************** SLIG7120 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*          ARQUIVO ARQDATAS VAZIO          *'
               DISPLAY '*                                          *'
               DISPLAY '*         PROCESSAMENTO  ENCERRADO         *'
               DISPLAY '*                                          *'
               DISPLAY '***************** SLIG6000 *****************'
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           ELSE
               IF WRK-FS-ARQDATAS      EQUAL '10' AND
                  ACU-DESPREZADOS      NOT EQUAL ZEROS
                  DISPLAY '***************** SLIG7120 *****************'
                  DISPLAY '*                                          *'
                  DISPLAY '*          REGISTRO NAO ENCONTRADO         *'
                  DISPLAY '*            NO ARQUIVO DE DATAS           *'
                  DISPLAY '*                                          *'
                  DISPLAY '*          PROCESSAMENTO ENCERRADO         *'
                  DISPLAY '*                                          *'
                  DISPLAY '***************** SLIG6000 *****************'
                  PERFORM 999999-PROCESSAR-ROTINA-ERRO
               END-IF
           END-IF.

           PERFORM 220000-LER-MOVTOCHQ.

           IF  WRK-FS-MOVTOCHQ        EQUAL 'S'
               IF ACU-LIDOS-MOVTOCHQ     EQUAL ZEROS
                  DISPLAY
                  '****************** SLIG7120 ******************'
                  DISPLAY
                  '*                                            *'
                  DISPLAY
                  '*          ARQUIVO MOVTOCHQ VAZIO            *'
                  DISPLAY
                  '*                                            *'
                  DISPLAY
                  '*            PROGRAMA CANCELADO              *'
                  DISPLAY
                  '*                                            *'
                  DISPLAY
                  '****************** SLIG7120 ******************'
                  MOVE WRK-MSG01          TO ERR-TEXTO
               ELSE
                  DISPLAY
                  '***************** SLIG7120 *****************'
                  DISPLAY
                  '*                                          *'
                  DISPLAY
                  '*  NENHUM EMITT /TP-PESSOA FOI SELECIONADO *'
                  DISPLAY
                  '*   VERIFICAR OS PARAMETROS INFORMADOS     *'
                  DISPLAY
                  '*                                          *'
                  DISPLAY
                  '***************** SLIG7120 *****************'
                  MOVE WRK-MSG04            TO ERR-TEXTO
               END-IF

               PERFORM 999999-PROCESSAR-ROTINA-ERRO

           END-IF.

      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       210000-LER-ARQDATAS             SECTION.
      *---------------------------------------------------------------*

           READ ARQDATAS.

           IF  WRK-FS-ARQDATAS         EQUAL '10'
               GO TO 210000-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 114000-TESTAR-FS-ARQDATAS.

           IF  SLIG00-CD-TP-PROCESSA   NOT EQUAL 2
               ADD 1                   TO ACU-DESPREZADOS
               GO TO 210000-LER-ARQDATAS
           ELSE

      *--  FORMATA PERIODO PARA TIPO 2 (DECENAL)  --------------------*

              MOVE SLIG00-DT-INI-PER-ATU    TO WRK-DT-PER-INI
              MOVE SLIG00-DT-FIM-PER-ATU    TO WRK-DT-PER-FIM

           END-IF.

      *---------------------------------------------------------------*
       210000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       220000-LER-MOVTOCHQ             SECTION.
      *---------------------------------------------------------------*

           CALL 'BRAD0300'             USING WRK-MOVTOCHQ
                                             REG-CHEQUE-EMITENTE-RELATO.

           IF  RETURN-CODE             EQUAL 4
               MOVE 'S'                TO WRK-FS-MOVTOCHQ
               GO TO 220000-99-FIM
           END-IF.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'BRAD0300'         TO WRK-NOME-POOL
               MOVE 'MOVTOCHQ'         TO WRK-NOME-ARQ
               MOVE  RETURN-CODE       TO WRK-RETURN-CODE
               MOVE  WRK-ERRO-TEXTO    TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *--  FORMATA CHAVE PARA SELECAO A PARTIR DO PARAMETRO  ---------*

           MOVE WRK-CHV-MOVTOCHQ       TO WRK-CHV-MOVTOCHQ-ANT.

           MOVE SLIG74-CGC-CPF-EMITT-ST
           TO WRK-CPF-MOVTOCHQ.

CNPJ7C     IF  SLIG74-CGC-CPF-EMITT-ST EQUAL '000000000' OR SPACES
DES            MOVE 'J'                TO WRK-TPS-MOVTOCHQ
DES                                       SLIG74-TIPO-PESSOA-EMITT
DES        ELSE
DES           IF  SLIG74-FILIAL-EMITT-ST  EQUAL '0000' OR SPACES
DES               MOVE 'F'             TO WRK-TPS-MOVTOCHQ
DES           ELSE
DES               MOVE 'J'             TO WRK-TPS-MOVTOCHQ
DES           END-IF
DES        END-IF.

      *--  ACUMULA LIDOS  --------------------------------------------*

           ADD  1                      TO ACU-LIDOS-MOVTOCHQ.

      *--  VERIFICA CLASSIFICACAO DO ARQUIVO DE ENTRADA  -------------*

           IF WRK-CHV-MOVTOCHQ      LESS WRK-CHV-MOVTOCHQ-ANT
                DISPLAY '***************** SLIG7120 *****************'
                DISPLAY '*                                          *'
                DISPLAY '*  ARQUIVO MOVTOCHQ COM ERRO DE SEQUENCIA  *'
                DISPLAY '*  CHAVE   ATUAL = ' WRK-CHV-MOVTOCHQ
                DISPLAY '*  CHAVE   ANTER = ' WRK-CHV-MOVTOCHQ-ANT
                DISPLAY '*                                          *'
                DISPLAY '***************** SLIG7120 *****************'
                MOVE WRK-MSG03            TO ERR-TEXTO
                PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *--  DESPREZA REGISTRO CASO CHAVE DIFERENTE DE PARAMETROS  -----*

           IF WRK-CHV-MOVTOCHQ NOT EQUAL LNK-PARM-CHV1 AND
                                         LNK-PARM-CHV2 AND
                                         LNK-PARM-CHV3 AND
                                         LNK-PARM-CHV4 AND
                                         LNK-PARM-CHV5
              ADD 1 TO ACU-EMITT-DESPR
              GO TO   220000-LER-MOVTOCHQ

           END-IF.

      *----------------------------------------------------------------*
       220000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       300000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 310000-GRAVAR-MVSELCHQ.
           PERFORM 220000-LER-MOVTOCHQ.

      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       310000-GRAVAR-MVSELCHQ          SECTION.
      *---------------------------------------------------------------*

           MOVE SPACES                 TO REG-SELECIONADO.

           INITIALIZE REG-SELECIONADO.

CNPJ7C     MOVE SLIG74-CGC-CPF-EMITT      TO SLIG62-CGC-CPF-CTRL.
           MOVE SLIG74-CGC-CPF-EMITT-ST   TO SLIG62-CGC-CPF-CTRL-ST.
           MOVE 'C'                    TO SLIG62-PRODUTO-CTRL.
           MOVE SLIG74-CARTEIRA        TO SLIG62-CARTEIRA-CTRL
                                          SLIG62-CHQ-CARTEIRA.
           MOVE SLIG74-BCO-EMITT       TO SLIG62-BANCO-EMITT
                                          SLIG62-CHQ-BCO-EMITT.
           MOVE SLIG74-AGENCIA-EMITT   TO SLIG62-AGENCIA-EMITT-CTRL
                                          SLIG62-AGEN-EMITT
                                          SLIG62-CHQ-AGENCIA-EMITT.
           MOVE SLIG74-CONTA-EMITT     TO SLIG62-CONTA-EMITT-CTRL
                                          SLIG62-CTA-EMITT
                                          SLIG62-CHQ-CONTA-EMITT.
           MOVE SLIG74-NUM-CHEQUE      TO SLIG62-NUM-CHEQUE-CTRL
                                          SLIG62-NUM-CHEQUE-EMITT
                                          SLIG62-CHQ-NUM-CHEQUE.
           MOVE SLIG74-DT-ENTRADA-ORIGEM
                                       TO SLIG62-DT-ENTRADA-CTRL.
           MOVE WRK-DT-PER-INI         TO SLIG62-DT-PER-INI.
           MOVE WRK-DT-PER-FIM         TO SLIG62-DT-PER-FIM.
CNPJ7C     MOVE SLIG74-CGC-CPF-EMITT    TO SLIG62-CHQ-CGC-CPF-EMITT
CNPJ7C     MOVE SLIG74-FILIAL-EMITT     TO SLIG62-CHQ-FILIAL-EMITT.
CNPJ7C     MOVE SLIG74-CONTROLE-EMITT
CNPJ7C                               TO SLIG62-CHQ-CONTROLE-EMITT.
           MOVE SLIG74-CGC-CPF-EMITT-ST TO SLIG62-CHQ-CGC-CPF-EMITT-ST
           MOVE SLIG74-FILIAL-EMITT-ST  TO SLIG62-CHQ-FILIAL-EMITT-ST.
           MOVE SLIG74-CONTROLE-EMITT-ST
                                     TO SLIG62-CHQ-CONTROLE-EMITT-ST.
           MOVE SLIG74-TIPO-PESSOA-EMITT
                                       TO SLIG62-CHQ-TIPO-PESSOA-EMITT.
           MOVE SLIG74-DIG-CHEQUE      TO SLIG62-CHQ-DIG-CHEQUE.
           MOVE SLIG74-NUM-LOTE        TO SLIG62-CHQ-NUM-LOTE.
           MOVE SLIG74-NOME-EMITT      TO SLIG62-CHQ-NOME-EMITT.
           MOVE SLIG74-BCO-CLI         TO SLIG62-CHQ-BCO-CLI.
           MOVE SLIG74-AGENCIA-CLI     TO SLIG62-CHQ-AGENCIA-CLI.
           MOVE SLIG74-CONTA-CLI       TO SLIG62-CHQ-CONTA-CLI.
CNPJ7C     MOVE SLIG74-CGC-CPF-CLI        TO SLIG62-CHQ-CGC-CPF-CLI.
CNPJ7C     MOVE SLIG74-FILIAL-CLI         TO SLIG62-CHQ-FILIAL-CLI.
CNPJ7C     MOVE SLIG74-CONTROLE-CLI       TO SLIG62-CHQ-CONTROLE-CLI.
           MOVE SLIG74-CGC-CPF-CLI-ST     TO SLIG62-CHQ-CGC-CPF-CLI-ST.
           MOVE SLIG74-FILIAL-CLI-ST      TO SLIG62-CHQ-FILIAL-CLI-ST.
           MOVE SLIG74-CONTROLE-CLI-ST    TO SLIG62-CHQ-CONTROLE-CLI-ST.
           MOVE SLIG74-TIPO-PESSOA-CLI TO SLIG62-CHQ-TIPO-PESSOA-CLI.
           MOVE SLIG74-NOME-CLI        TO SLIG62-CHQ-NOME-CLI.
           MOVE SLIG74-DT-DEPOSITO     TO SLIG62-CHQ-DT-DEPOSITO.
           MOVE SLIG74-DT-CONTABIL     TO SLIG62-CHQ-DT-CONTABIL.
           MOVE SLIG74-DT-ENTRADA-ORIGEM
                                       TO SLIG62-CHQ-DT-ENTRADA-ORIGEM.
           MOVE SLIG74-DT-SITUACAO     TO SLIG62-CHQ-DT-SITUACAO.
           MOVE SLIG74-CTPO-CHEQ       TO SLIG62-CHQ-CTPO-CHEQ.
           MOVE SLIG74-VL-PRINC-CHEQ   TO SLIG62-CHQ-VL-PRINC-CHEQ.
           MOVE SLIG74-BCO-DIR-REGIONAL
                                       TO SLIG62-CHQ-BCO-DIR-REGIONAL.
           MOVE SLIG74-DIR-REGIONAL    TO SLIG62-CHQ-DIR-REGIONAL.
           MOVE SLIG74-BCO-GERC-REGIONAL
                                       TO SLIG62-CHQ-BCO-GERC-REGIONAL.
           MOVE SLIG74-GERC-REGIONAL   TO SLIG62-CHQ-GERC-REGIONAL.
           MOVE SLIG74-GRP-ECONOMICO-EMITT
                                      TO SLIG62-CHQ-GRP-ECONOMICO-EMITT.
           MOVE SLIG74-GRP-ECONOMICO-CLI
                                       TO SLIG62-CHQ-GRP-ECONOMICO-CLI.
SO         MOVE SLIG74-CODI-DEVOL-CHQ
                                       TO SLIG62-CODI-DEVOL-CHQ.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           WRITE REG-SELECIONADO.

           PERFORM 112000-TESTAR-FS-MVSELCHQ.

           ADD 1                       TO ACU-GRAVA-MVSELCHQ.

      *----------------------------------------------------------------*
       310000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       400000-FINALIZAR                SECTION.
      *----------------------------------------------------------------*


           PERFORM 410000-EMITIR-SRELCTRL.

           CLOSE ARQDATAS
                 MVSELCHQ
                 SRELCTRL.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 110000-TESTAR-FILE-STATUS.

           MOVE ZEROS                  TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       400000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       410000-EMITIR-SRELCTRL          SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL7600'             USING WRK-DATA-HORA.

           MOVE WRK-DIA-SIS            TO CB2-DIA.
           MOVE WRK-MES-SIS            TO CB2-MES.
           MOVE WRK-ANO-SIS            TO CB2-ANO.
           MOVE ACU-LIDOS-MOVTOCHQ     TO LT2-LIDOS.
           MOVE ACU-GRAVA-MVSELCHQ     TO LT3-GRAVADOS.
           MOVE ACU-EMITT-DESPR        TO LT4-N-ENCONT.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE REG-SRELCTRL          FROM CABEC1.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM CABEC2.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM CABEC3.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT2.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT3.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT4.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

      *----------------------------------------------------------------*
       410000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       999999-PROCESSAR-ROTINA-ERRO    SECTION.
      *----------------------------------------------------------------*

           MOVE 'APL'                  TO ERR-TIPO-ACESSO.
           MOVE 'SLIG7120'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       999999-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
