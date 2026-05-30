      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT6875.
       AUTHOR.     MARCIO ALEXANDRO RUI.
      *================================================================*
      *            B A N C O   B R A D E S C O  - S A                  *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT6875                                    *
      *    PROGRAMADOR.:   MARCIO ALEXANDRO RUI                        *
      *    ANALISTA....:   MARCIO ALEXANDRO RUI                        *
      *    DATA........:   AGOSTO/2019                                 *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   - VERIFICA A VALIDADE DO TESTE DRIVE        *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                  DDNAME                        INCLUDE/BOOK    *
      *                    CADTESTD                      GFCTW003      *
      *                    ISETESTD                      GFCTW003      *
      *                    RELATO                            -         *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    POL7100C - PARAMETROS DO MODULO POOL7100                    *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL7100 - TRATAMENTO DE ERRO QUANDO PROGRAMA INVALIDO      *
      *    POOL7600 - FORNECE DATA E HORA DO SISTEMA                   *
      *================================================================*
      *    ALTERACOES                                                  *
      *================================================================*
RU0620*    06/2020 - MARCIO ALEXANDRO RUI                              *
RU0620*              PARCERIA CAASP - 2020-0617521-5-002               *
RU0620*              RECOMPILACAO ALTERACAO COPY GFCTW003              *
RU0620*================================================================*
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

           SELECT CADTESTD ASSIGN      TO UT-S-CADTESTD
           FILE   STATUS               IS WRK-FS-CADTESTD.

           SELECT ISETESTD ASSIGN      TO UT-S-ISETESTD
           FILE   STATUS               IS WRK-FS-ISETESTD.

           SELECT RELATO ASSIGN        TO UT-S-RELATO
           FILE   STATUS               IS WRK-FS-RELATO.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE ENTRADA - CONTAS EM TEST DRIVE           *
      *            ORG. SEQUENCIAL     -    LRECL = 200                *
      *----------------------------------------------------------------*

       FD  CADTESTD
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY GFCTW003.

      *----------------------------------------------------------------*
      *   OUTPUT:  ARQUIVO DE SAIDA - CONTAS EM TEST DRIVE             *
      *            ORG. SEQUENCIAL     -    LRECL = 200                *
      *----------------------------------------------------------------*

       FD  ISETESTD
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY GFCTW003.

      *----------------------------------------------------------------*
      *   OUTPUT:  RELATORIO DE REGISTROS BASE PARA TESTE DRIVE        *
      *            ORG. SEQUENCIAL     -    LRECL = 133                *
      *----------------------------------------------------------------*

       FD  RELATO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-RELATO               PIC  X(133).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** GFCT6875 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** ACUMULADORES ***'.
      *----------------------------------------------------------------*

       77  ACU-LINHAS-RELATO           PIC  9(002) COMP-3  VALUE 99.
       77  ACU-PAG-RELATO              PIC  9(005) COMP-3  VALUE ZEROS.
       77  ACU-LID-CADTESTD            PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-DES-CADTESTD            PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRA-ISETESTD            PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA TESTES DE FILE STATUS ***'.
      *----------------------------------------------------------------*

       77  WRK-FS-CADTESTD             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ISETESTD             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-RELATO               PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA  '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-MASCARA                 PIC  Z.ZZZ.ZZZ.ZZ9 VALUE ZEROS.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

       01  WRK-DT-INCLUSAO             PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-INCLUSAO-R           REDEFINES WRK-DT-INCLUSAO.
           03  WRK-ANO-INCLUSAO        PIC  9(004).
           03  WRK-MES-INCLUSAO        PIC  9(002).
           03  WRK-DIA-INCLUSAO        PIC  9(002).

       01  WRK-DT-FIM-VALIDADE         PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-FIM-VALIDADE-R       REDEFINES WRK-DT-FIM-VALIDADE.
           03  WRK-ANO-FIM-VALIDADE    PIC  9(004).
           03  WRK-MES-FIM-VALIDADE    PIC  9(002).
           03  WRK-DIA-FIM-VALIDADE    PIC  9(002).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ERRO - FILE-STATUS ***'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-TEXTO.
           03  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           03  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           03  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           03  WRK-NOME-ARQUIVO        PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(017)         VALUE
               ' - FILE-STATUS = '.
           03  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA POOL7600 ***'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           03  WRK-DT-JULIANA          PIC  9(005) COMP-3  VALUE ZEROS.
           03  WRK-DT-AAMMDD           PIC  9(007) COMP-3  VALUE ZEROS.
RU0919     03  WRK-DT-AAAAMMDD         PIC  9(008) COMP-3  VALUE ZEROS.
           03  WRK-TI-HHMMSS           PIC  9(007) COMP-3  VALUE ZEROS.
           03  WRK-TI-HHMMSSMMMMMM     PIC  9(013) COMP-3  VALUE ZEROS.
           03  WRK-TIMESTAMP.
               05  WRK-ANO             PIC  9(004)         VALUE ZEROS.
               05  WRK-MES             PIC  9(002)         VALUE ZEROS.
               05  WRK-DIA             PIC  9(002)         VALUE ZEROS.
               05  WRK-HORA            PIC  9(002)         VALUE ZEROS.
               05  WRK-MIN             PIC  9(002)         VALUE ZEROS.
               05  WRK-SEG             PIC  9(002)         VALUE ZEROS.
               05  FILLER              PIC  X(006)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA CABECALHOS ***'.
      *----------------------------------------------------------------*

       01  CB1-REL1.
           03  FILLER                  PIC  X(001)         VALUE '1'.
           03  FILLER                  PIC  X(051)         VALUE
               'GFCT6875'.
           03  FILLER                  PIC  X(071)         VALUE
               'BANCO BRADESCO S.A.'.
           03  FILLER                  PIC  X(004)         VALUE 'PAG.'.
           03  CB1-PAG                 PIC  ZZ.ZZ9         VALUE ZEROS.

       01  CB3-REL1.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  CB3-DIA                 PIC  99             VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB3-MES                 PIC  99             VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB3-ANO                 PIC  9999           VALUE ZEROS.
           03  FILLER                  PIC  X(030)         VALUE SPACES.
           03  FILLER                  PIC  X(080)         VALUE
               'RELACAO DE CLIENTES EM PERIODO DE TESTE DRIVE'.
           03  CB3-HOR                 PIC  99             VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE ':'.
           03  CB3-MIN                 PIC  99             VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE ':'.
           03  CB3-SEG                 PIC  99             VALUE ZEROS.

       01  CB4-REL1.
           03  FILLER                  PIC  X(001)         VALUE '-'.
           03  FILLER                  PIC  X(005)         VALUE SPACES.
           03  FILLER                  PIC  X(024)         VALUE 'CPF'.
           03  FILLER                  PIC  X(012)         VALUE
               'AGENCIA'.
           03  FILLER                  PIC  X(018)         VALUE
               'RAZAO'.
           03  FILLER                  PIC  X(010)         VALUE
               'CONTA'.
           03  FILLER                  PIC  X(013)         VALUE
               'CESTA   '.
           03  FILLER                  PIC  X(018)         VALUE
               'DATA INCLUSAO'.
           03  FILLER                  PIC  X(016)         VALUE
               'MESES DRIVE'.
           03  FILLER                  PIC  X(017)         VALUE
               'DATA FIM VALIDADE'.

       01  CB5-REL1.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(132)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA LINHA DE DETALHE ***'.
      *----------------------------------------------------------------*

       01  LD1-REL1.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(005)         VALUE SPACES.
ST25X6*           03  LD1-CPF-NUM             PIC  999.999.999    VALUE ZEROS.
           03  LD1-CPF-NUM             PIC  X(009)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE '/'.
ST25X6*           03  LD1-CPF-FIL             PIC  99999          VALUE ZEROS.
           03  LD1-CPF-FIL             PIC  X(004)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE '-'.
ST25X6*           03  LD1-CPF-CTRL            PIC  99             VALUE ZEROS.
           03  LD1-CPF-CTRL            PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(006)         VALUE SPACES.
           03  LD1-AGENCIA             PIC  ZZZZ9          VALUE ZEROS.
           03  FILLER                  PIC  X(005)         VALUE SPACES.
           03  LD1-RAZAO.
               05  LD1-GRUPO-RAZAO     PIC  9(005)         VALUE ZEROS.
           03  FILLER                  PIC  X(004)         VALUE SPACES.
           03  LD1-CONTA               PIC  ZZZZZZZZZZZZ9  VALUE ZEROS.
           03  FILLER                  PIC  X(006)         VALUE SPACES.
           03  LD1-CESTA               PIC  99999          VALUE ZEROS.
           03  FILLER                  PIC  X(007)         VALUE SPACES.
           03  LD1-DIA-INCLUSAO        PIC  99             VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  LD1-MES-INCLUSAO        PIC  99             VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  LD1-ANO-INCLUSAO        PIC  9999           VALUE ZEROS.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  LD1-MESES-DRIVE         PIC  999            VALUE ZEROS.
           03  FILLER                  PIC  X(012)         VALUE SPACES.
           03  LD1-DIA-FIM-VALIDADE    PIC  99             VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  LD1-MES-FIM-VALIDADE    PIC  99             VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  LD1-ANO-FIM-VALIDADE    PIC  9999           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA LINHAS DE TOTAIS ***'.
      *----------------------------------------------------------------*

       01  LD2-REL1.
           03  FILLER                  PIC  X(001)         VALUE '-'.
           03  FILLER                  PIC  X(018)         VALUE SPACES.
           03  FILLER                  PIC  X(028)         VALUE
               'TOTAL DE REGISTROS LIDOS'.
           03  FILLER                  PIC  X(022)         VALUE
               'EM CADTESTD.......... '.
           03  LD2-TOT-LID-CADTESTD    PIC  Z.ZZZ.ZZZ.ZZ9  VALUE ZEROS.

       01  LD3-REL1.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(018)         VALUE SPACES.
           03  FILLER                  PIC  X(050)         VALUE
               'TOTAL DE REGISTROS GRAVADOS EM ISETESTD.......... '.
           03  LD3-TOT-GRAV-ISETESTD   PIC  Z.ZZZ.ZZZ.ZZ9  VALUE ZEROS .

       01  LD4-REL1.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(018)         VALUE SPACES.
           03  FILLER                  PIC  X(050)         VALUE
               'TOTAL DE REGISTROS DESPREZADOS................... '.
           03  LD4-TOT-DESP-CADTESTD   PIC  Z.ZZZ.ZZZ.ZZ9  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA POOL7100 ***'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** GFCT6875 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-INICIAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 4000-PROCESSAR             UNTIL
               WRK-FS-CADTESTD                  EQUAL '10'.

           PERFORM 5000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIALIZAR              SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT CADTESTD
               OUTPUT ISETESTD
                      RELATO.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           CALL 'POOL7600'             USING WRK-DATA-HORA.

           MOVE WRK-DIA                TO CB3-DIA.
           MOVE WRK-MES                TO CB3-MES.
           MOVE WRK-ANO                TO CB3-ANO.
           MOVE WRK-HORA               TO CB3-HOR.
           MOVE WRK-MIN                TO CB3-MIN.
           MOVE WRK-SEG                TO CB3-SEG.

      *----------------------------------------------------------------*
       1000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-CADTESTD.

           PERFORM 1130-TESTAR-FS-ISETESTD.

           PERFORM 1150-TESTAR-FS-RELATO.

      *----------------------------------------------------------------*
       1100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1110-TESTAR-FS-CADTESTD       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CADTESTD         NOT EQUAL ZEROS
               MOVE 'CADTESTD'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-CADTESTD   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-TEXTO    TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1130-TESTAR-FS-ISETESTD       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ISETESTD         NOT EQUAL ZEROS
               MOVE 'ISETESTD'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-ISETESTD   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-TEXTO    TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1150-TESTAR-FS-RELATO       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-RELATO         NOT EQUAL ZEROS
               MOVE 'RELATO'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-RELATO   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-TEXTO    TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1150-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO          SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-CADTESTD.

           IF  WRK-FS-CADTESTD         EQUAL '10'
               DISPLAY '***************** GFCT6875 ******************'
               DISPLAY '*                                           *'
               DISPLAY '*           ARQUIVO CADTESTD VAZIO          *'
               DISPLAY '*                                           *'
               DISPLAY '***************** GFCT6875 ******************'
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-LER-CADTESTD             SECTION.
      *----------------------------------------------------------------*

           READ CADTESTD.

           IF  WRK-FS-CADTESTD         EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.
           PERFORM 1110-TESTAR-FS-CADTESTD.

           ADD 1                       TO ACU-LID-CADTESTD.

           IF  GFCT0003-DFIM-TST-DRIVE OF CADTESTD
                                       LESS  WRK-DT-AAAAMMDD
               ADD  1                  TO ACU-DES-CADTESTD
               GO  TO  2100-LER-CADTESTD
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-GRAVAR-ISETESTD.

           PERFORM 4200-IMPRIMIR-RELATO.

           PERFORM 2100-LER-CADTESTD.

      *----------------------------------------------------------------*
       4000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4100-GRAVAR-ISETESTD          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT0003-REGISTRO OF CADTESTD
                                   TO GFCT0003-REGISTRO OF ISETESTD.

           MOVE WRK-GRAVACAO             TO WRK-OPERACAO.
           WRITE  GFCT0003-REGISTRO OF ISETESTD
           PERFORM 1130-TESTAR-FS-ISETESTD.

           ADD 1                         TO ACU-GRA-ISETESTD.

      *----------------------------------------------------------------*
       4100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4200-IMPRIMIR-RELATO        SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LINHAS-RELATO     GREATER 60
               PERFORM 4210-IMPRIMIR-CABEC-RELATO
           END-IF.

           MOVE GFCT0003-CPF-CNPJ OF CADTESTD
                                       TO LD1-CPF-NUM.
           MOVE GFCT0003-FILIAL   OF CADTESTD
                                       TO LD1-CPF-FIL.
           MOVE GFCT0003-CONTROLE OF CADTESTD
                                       TO LD1-CPF-CTRL.
           MOVE GFCT0003-CJUNC-DEPDC OF CADTESTD
                                       TO LD1-AGENCIA.
           MOVE GFCT0003-RAZAO       OF CADTESTD
                                       TO LD1-GRUPO-RAZAO.
           MOVE GFCT0003-CCTA-CORR   OF CADTESTD
                                       TO LD1-CONTA.
           MOVE GFCT0003-CESTA-SERVICO OF CADTESTD
                                       TO LD1-CESTA.
           MOVE GFCT0003-DINIC-TST-DRIVE OF CADTESTD
                                       TO WRK-DT-INCLUSAO.
           MOVE WRK-ANO-INCLUSAO       TO LD1-ANO-INCLUSAO.
           MOVE WRK-MES-INCLUSAO       TO LD1-MES-INCLUSAO.
           MOVE WRK-DIA-INCLUSAO       TO LD1-DIA-INCLUSAO.
           MOVE GFCT0003-PRAZO-ISENCAO OF CADTESTD
                                       TO LD1-MESES-DRIVE.
           MOVE GFCT0003-DFIM-TST-DRIVE OF CADTESTD
                                       TO WRK-DT-FIM-VALIDADE.
           MOVE WRK-ANO-FIM-VALIDADE   TO LD1-ANO-FIM-VALIDADE.
           MOVE WRK-MES-FIM-VALIDADE   TO LD1-MES-FIM-VALIDADE.
           MOVE WRK-DIA-FIM-VALIDADE   TO LD1-DIA-FIM-VALIDADE.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE FD-REG-RELATO         FROM LD1-REL1.

           PERFORM 1150-TESTAR-FS-RELATO.

           ADD 1                       TO ACU-LINHAS-RELATO.

      *----------------------------------------------------------------*
       4200-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4210-IMPRIMIR-CABEC-RELATO  SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO ACU-PAG-RELATO.

           MOVE ACU-PAG-RELATO         TO CB1-PAG.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE FD-REG-RELATO          FROM CB1-REL1.
           PERFORM 1150-TESTAR-FS-RELATO.

           WRITE FD-REG-RELATO          FROM CB3-REL1.
           PERFORM 1150-TESTAR-FS-RELATO.

           WRITE FD-REG-RELATO          FROM CB4-REL1.
           PERFORM 1150-TESTAR-FS-RELATO.

           WRITE FD-REG-RELATO          FROM CB5-REL1.
           PERFORM 1150-TESTAR-FS-RELATO.

           MOVE 6                      TO ACU-LINHAS-RELATO.

      *----------------------------------------------------------------*
       4210-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5000-FINALIZAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 5100-EMITIR-TOTAIS-RELATO.

           PERFORM 5300-EMITIR-ESTATISTICAS.

           CLOSE CADTESTD
                 ISETESTD
                 RELATO.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.
           PERFORM 1100-TESTAR-FILE-STATUS.

           GOBACK.

      *----------------------------------------------------------------*
       5000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5100-EMITIR-TOTAIS-RELATO   SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           IF  ACU-LINHAS-RELATO     GREATER 50
               ADD 1                   TO ACU-PAG-RELATO
               MOVE ACU-PAG-RELATO   TO CB1-PAG
               WRITE FD-REG-RELATO   FROM CB1-REL1
               PERFORM 1150-TESTAR-FS-RELATO
               WRITE FD-REG-RELATO   FROM CB3-REL1
               PERFORM 1150-TESTAR-FS-RELATO
           END-IF.

           MOVE ACU-LID-CADTESTD       TO LD2-TOT-LID-CADTESTD.
           MOVE ACU-GRA-ISETESTD       TO LD3-TOT-GRAV-ISETESTD.
           MOVE ACU-DES-CADTESTD       TO LD4-TOT-DESP-CADTESTD.

           WRITE FD-REG-RELATO         FROM CB5-REL1.
           PERFORM 1150-TESTAR-FS-RELATO.

           WRITE FD-REG-RELATO         FROM CB5-REL1.
           PERFORM 1150-TESTAR-FS-RELATO.

           WRITE FD-REG-RELATO         FROM CB5-REL1.
           PERFORM 1150-TESTAR-FS-RELATO.

           WRITE FD-REG-RELATO         FROM LD2-REL1.
           PERFORM 1150-TESTAR-FS-RELATO.

           WRITE FD-REG-RELATO         FROM LD3-REL1.
           PERFORM 1150-TESTAR-FS-RELATO.

           WRITE FD-REG-RELATO         FROM LD4-REL1.
           PERFORM 1150-TESTAR-FS-RELATO.

      *----------------------------------------------------------------*
       5100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       5300-EMITIR-ESTATISTICAS      SECTION.
      *----------------------------------------------------------------*

           DISPLAY '***************** GFCT6875 ******************'.
           DISPLAY '*                                           *'.

           MOVE ACU-LID-CADTESTD       TO WRK-MASCARA
           DISPLAY '* TOTAL LIDOS       CADTESTD: ' WRK-MASCARA ' *'.

           MOVE ACU-DES-CADTESTD       TO WRK-MASCARA
           DISPLAY '* TOTAL DESPREZADOS CADTESTD: ' WRK-MASCARA ' *'.

           MOVE ACU-GRA-ISETESTD       TO WRK-MASCARA
           DISPLAY '* TOTAL GRAVADOS    ISETESTD: ' WRK-MASCARA ' *'.

           DISPLAY '*                                           *'.
           DISPLAY '***************** GFCT6875 ******************'.

      *----------------------------------------------------------------*
       5300-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO    SECTION.
      *----------------------------------------------------------------*

           MOVE 'APL'                  TO ERR-TIPO-ACESSO.
           MOVE 'GFCT6875'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                  EXIT.
      *---------------------------------------------------------------*
