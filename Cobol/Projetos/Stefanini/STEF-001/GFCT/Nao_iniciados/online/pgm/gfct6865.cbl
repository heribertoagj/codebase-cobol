      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT6865.
       AUTHOR.     CRISTHIAN THOMAZ PATULSKI JUNIOR.
      *================================================================*
      *                    BRADESCO - DS                               *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT6865                                    *
      *    ANALISTA....:   CRISTHIAN PATULSKI JR.                      *
      *    DATA........:   03/09/2019                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR ARQUIVO BASE PARA TESTE DRIVE AVON.   *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                  DDNAME                        INCLUDE/BOOK    *
      *                    ARQSEGTO                      GFCTW001      *
      *                    SEGMOVTO                      GFCTW003      *
      *                    ARQDRIVE                      GFCTW003      *
      *                    ARQDESPR                      GFCTW003      *
      *                    RELDESPR                          -         *
      *                    RELDRIVE                          -         *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    POL7100C - PARAMETROS DO MODULO POOL7100                    *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL7100 - TRATAMENTO DE ERRO QUANDO PROGRAMA INVALIDO      *
      *    POOL7600 - FORNECE DATA E HORA DO SISTEMA                   *
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

           SELECT ARQSEGTO ASSIGN      TO UT-S-ARQSEGTO
           FILE   STATUS               IS WRK-FS-ARQSEGTO.

           SELECT SEGMOVTO ASSIGN      TO UT-S-SEGMOVTO
           FILE   STATUS               IS WRK-FS-SEGMOVTO.

           SELECT ARQDRIVE ASSIGN      TO UT-S-ARQDRIVE
           FILE   STATUS               IS WRK-FS-ARQDRIVE.

           SELECT ARQDESPR ASSIGN      TO UT-S-ARQDESPR
           FILE   STATUS               IS WRK-FS-ARQDESPR.

           SELECT RELDESPR ASSIGN      TO UT-S-RELDESPR
           FILE   STATUS               IS WRK-FS-RELDESPR.

           SELECT RELDRIVE ASSIGN      TO UT-S-RELDRIVE
           FILE   STATUS               IS WRK-FS-RELDRIVE.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE ENTRADA ARQSEGTO  GFCTW001               *
      *            ORG. SEQUENCIAL     -    LRECL = 255                *
      *----------------------------------------------------------------*

       FD  ARQSEGTO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY GFCTW001.
      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE ENTRADA SEGMOVTO                         *
      *            ORG. SEQUENCIAL     -    LRECL = 200                *
      *----------------------------------------------------------------*

       FD  SEGMOVTO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY GFCTW003.
      *----------------------------------------------------------------*
      *   OUTPUT:  ARQUIVO DE SAIDA ARQDRIVE                           *
      *            ORG. SEQUENCIAL     -    LRECL = 200                *
      *----------------------------------------------------------------*

       FD  ARQDRIVE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY GFCTW003.
      *----------------------------------------------------------------*
      *   OUTPUT:  ARQUIVO DE SAIDA ARQDESPR                           *
      *            ORG. SEQUENCIAL     -    LRECL = 200                *
      *----------------------------------------------------------------*

       FD  ARQDESPR
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY GFCTW003.
      *----------------------------------------------------------------*
      *   OUTPUT:  RELATORIO DE REGISTROS DESPREZADOS                  *
      *            ORG. SEQUENCIAL     -    LRECL = 133                *
      *----------------------------------------------------------------*

       FD  RELDESPR
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-RELDESPR             PIC  X(133).

      *----------------------------------------------------------------*
      *   OUTPUT:  RELATORIO DE REGISTROS BASE PARA TESTE DRIVE        *
      *            ORG. SEQUENCIAL     -    LRECL = 133                *
      *----------------------------------------------------------------*

       FD  RELDRIVE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-RELDRIVE             PIC  X(133).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** GFCT6865 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** INDEXADOR ***'.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** ACUMULADORES ***'.
      *----------------------------------------------------------------*

       77  ACU-LINHAS-RELDRIVE         PIC  9(002) COMP-3  VALUE 99.
       77  ACU-LINHAS-RELDESPR         PIC  9(002) COMP-3  VALUE 99.
       77  ACU-PAG-RELDRIVE            PIC  9(005) COMP-3  VALUE ZEROS.
       77  ACU-PAG-RELDESPR            PIC  9(005) COMP-3  VALUE ZEROS.
       77  ACU-LID-ARQSEGTO            PIC  9(010) COMP-3  VALUE ZEROS.
       77  ACU-LID-SEGMOVTO            PIC  9(010) COMP-3  VALUE ZEROS.
       77  ACU-DES-ARQSEGTO            PIC  9(010) COMP-3  VALUE ZEROS.
       77  ACU-GRA-ARQDRIVE            PIC  9(010) COMP-3  VALUE ZEROS.
       77  ACU-GRA-ARQDESPR            PIC  9(010) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA TESTES DE FILE STATUS ***'.
      *----------------------------------------------------------------*

       77  WRK-FS-ARQSEGTO             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SEGMOVTO             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ARQDRIVE             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ARQDESPR             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-RELDESPR             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-RELDRIVE             PIC  X(002)         VALUE SPACES.

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

       77  WRK-MASCARA                 PIC  Z.ZZZ.ZZZ.ZZ9  VALUE ZEROS.
ST25X6*       77  WRK-CPF-ANT                 PIC  9(009)         VALUE ZEROS.
       77  WRK-CPF-ANT                 PIC  X(009)         VALUE SPACES.
       77  WRK-MESES-DRIVE             PIC  9(003)         VALUE ZEROS.
       77  WRK-MESES-AUX               PIC  9(002) COMP-3  VALUE ZEROS.
       77  WRK-CESTA-SERV              PIC  9(005) COMP-3  VALUE ZEROS.
       77  WRK-COD-GFCT                PIC  9(005) COMP-3  VALUE ZEROS.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-RESULTADO               PIC  9(004) COMP-3  VALUE ZEROS.
       77  WRK-RESTO                   PIC  9(001) COMP-3  VALUE ZEROS.
       77  WRK-MSG01                   PIC  X(075)         VALUE
           '** ESTOURO NA TABELA DE MESES **'.

       01  WRK-DT-INCLUSAO             PIC  X(010)         VALUE SPACES.
       01  WRK-DT-INCLUSAO-R           REDEFINES WRK-DT-INCLUSAO.
           03  WRK-DIA-INCLUSAO        PIC  9(002).
           03  FILLER                  PIC  X(001).
           03  WRK-MES-INCLUSAO        PIC  9(002).
           03  FILLER                  PIC  X(001).
           03  WRK-ANO-INCLUSAO        PIC  9(004).

       01  WRK-DT-FIM-VALIDADE         PIC  X(010)         VALUE SPACES.
       01  WRK-DT-FIM-VALIDADE-R       REDEFINES WRK-DT-FIM-VALIDADE.
           03  WRK-DIA-FIM-VALIDAUX    PIC  9(002).
           03  FILLER                  PIC  X(001).
           03  WRK-MES-FIM-VALIDAUX    PIC  9(002).
           03  FILLER                  PIC  X(001).
           03  WRK-ANO-FIM-VALIDAUX    PIC  9(004).

       01  WRK-DIA-FIM-VALIDADE        PIC  9(002) COMP-3 VALUE ZEROS.
       01  WRK-MES-FIM-VALIDADE        PIC  9(002) COMP-3 VALUE ZEROS.
       01  WRK-ANO-FIM-VALIDADE        PIC  9(004) COMP-3 VALUE ZEROS.

       01  WRK-DT-INCLUSAI             PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-INCLUSAI-R           REDEFINES WRK-DT-INCLUSAI.
           03  WRK-ANO-INCLUSAI        PIC  9(004).
           03  WRK-MES-INCLUSAI        PIC  9(002).
           03  WRK-DIA-INCLUSAI        PIC  9(002).

       01  WRK-DT-FIM-VALSAI           PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-FIM-VALSAI-R       REDEFINES WRK-DT-FIM-VALSAI.
           03  WRK-ANO-FIM-VALSAI      PIC  9(004).
           03  WRK-MES-FIM-VALSAI      PIC  9(002).
           03  WRK-DIA-FIM-VALSAI      PIC  9(002).

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
           03  WRK-DT-AAAAMMDD         PIC  9(009) COMP-3  VALUE ZEROS.
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
           '*** AREA PARA TABELA INTERNA DE MESES ***'.
      *----------------------------------------------------------------*

       01  TAB-MESES.
           03  FILLER                  OCCURS 9999 TIMES.
               05  TAB-CESTA-SERV      PIC  9(005)         VALUE ZEROS.
               05  TAB-MESES-DRIVE     PIC  9(002)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA WRK-CALHOS ***'.
      *----------------------------------------------------------------*

       01  WRK-C1.
           03  FILLER                  PIC  X(001)         VALUE '1'.
           03  FILLER                  PIC  X(051)         VALUE
               'GFCT6865'.
           03  FILLER                  PIC  X(071)         VALUE
               'BANCO BRADESCO S.A.'.
           03  FILLER                  PIC  X(004)         VALUE 'PAG.'.
           03  CB1-PAG                 PIC  ZZ.ZZ9         VALUE ZEROS.

       01  WRK-C2.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  CB2-DIA                 PIC  99             VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB2-MES                 PIC  99             VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB2-ANO                 PIC  9999           VALUE ZEROS.
           03  FILLER                  PIC  X(017)         VALUE SPACES.
           03  FILLER                  PIC  X(054)         VALUE
               'RELATORIO DE CONTAS DESPREZADAS PARA GERACAO DO ARQUIV'.
           03  FILLER                  PIC  X(043)         VALUE
               'O TESTE DRIVE'.
           03  CB2-HOR                 PIC  99             VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE ':'.
           03  CB2-MIN                 PIC  99             VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE ':'.
           03  CB2-SEG                 PIC  99             VALUE ZEROS.

       01  WRK-C3.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  CB3-DIA                 PIC  99             VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB3-MES                 PIC  99             VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB3-ANO                 PIC  9999           VALUE ZEROS.
           03  FILLER                  PIC  X(020)         VALUE SPACES.
           03  FILLER                  PIC  X(054)         VALUE
               'RELATORIO DE CONTAS GERADAS NO ARQUIVO BASE PARA TESTE'.
           03  FILLER                  PIC  X(036)         VALUE
               ' DRIVE'.
           03  CB3-HOR                 PIC  99             VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE ':'.
           03  CB3-MIN                 PIC  99             VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE ':'.
           03  CB3-SEG                 PIC  99             VALUE ZEROS.

       01  WRK-C4.
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
               'SEGMENTO'.
           03  FILLER                  PIC  X(018)         VALUE
               'DATA INCLUSAO'.
           03  FILLER                  PIC  X(016)         VALUE
               'MESES DRIVE'.
           03  FILLER                  PIC  X(017)         VALUE
               'DATA FIM VALIDADE'.

       01  WRK-C5.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(132)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA LINHA DE DETALHE ***'.
      *----------------------------------------------------------------*

       01  WRK-LIN-DET1.
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
           03  FILLER                  PIC  X(005)         VALUE SPACES.
           03  LD1-AGENCIA             PIC  ZZZZ9          VALUE ZEROS.
           03  FILLER                  PIC  X(007)         VALUE SPACES.
           03  LD1-RAZAO               PIC  9(005)         VALUE ZEROS.
           03  FILLER                  PIC  X(005)         VALUE SPACES.
           03  LD1-CONTA               PIC  ZZZZZZZZZZZZ9  VALUE ZEROS.
           03  FILLER                  PIC  X(006)         VALUE SPACES.
           03  LD1-CESTA-SERV          PIC  99999          VALUE ZEROS.
           03  FILLER                  PIC  X(009)         VALUE SPACES.
           03  LD1-DIA-INCLUSAO        PIC  99             VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  LD1-MES-INCLUSAO        PIC  99             VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  LD1-ANO-INCLUSAO        PIC  9999           VALUE ZEROS.
           03  FILLER                  PIC  X(009)         VALUE SPACES.
           03  LD1-MESES-DRIVE         PIC  999            VALUE ZEROS.
           03  FILLER                  PIC  X(014)         VALUE SPACES.
           03  LD1-DIA-FIM-VALIDADE    PIC  99             VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  LD1-MES-FIM-VALIDADE    PIC  99             VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  LD1-ANO-FIM-VALIDADE    PIC  9999           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA LINHAS DE TOTAIS ***'.
      *----------------------------------------------------------------*

       01  WRK-LIN-TOT1.
           03  FILLER                  PIC  X(001)         VALUE '-'.
           03  FILLER                  PIC  X(018)         VALUE SPACES.
           03  FILLER                  PIC  X(028)         VALUE
               'TOTAL DE REGISTROS LIDOS'.
           03  FILLER                  PIC  X(022)         VALUE
               'EM ARQSEGTO.......... '.
           03  WRK-TOT-LARQSEG         PIC  Z.ZZZ.ZZZ.ZZ9  VALUE ZEROS.

       01  WRK-LIN-TOT1A.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(018)         VALUE SPACES.
           03  FILLER                  PIC  X(028)         VALUE
               'TOTAL DE REGISTROS LIDOS'.
           03  FILLER                  PIC  X(022)         VALUE
               'EM SEGMOVTO.......... '.
           03  WRK-TOT-LSEGMOV          PIC  Z.ZZZ.ZZZ.ZZ9 VALUE ZEROS.

       01  WRK-LIN-TOT2.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(018)         VALUE SPACES.
           03  FILLER                  PIC  X(050)         VALUE
               'TOTAL DE REGISTROS GRAVADOS EM ARQDRIVE.......... '.
           03  WRK-TOT-GARQDRIV        PIC  Z.ZZZ.ZZZ.ZZ9  VALUE ZEROS.

       01  WRK-LIN-TOT2A.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(018)         VALUE SPACES.
           03  FILLER                  PIC  X(050)         VALUE
               'TOTAL DE REGISTROS GRAVADOS EM ARQDESPR.......... '.
           03  WRK-TOT-GARQDE           PIC  Z.ZZZ.ZZZ.ZZ9 VALUE ZEROS.

       01  WRK-LIN-TOT3.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(018)         VALUE SPACES.
           03  FILLER                  PIC  X(050)         VALUE
               'TOTAL DE REGISTROS DESPREZADOS................... '.
           03  WRK-TOT-DARQSEG         PIC  Z.ZZZ.ZZZ.ZZ9  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA POOL7100 ***'.
      *----------------------------------------------------------------*

           COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** GFCT6865 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-CARREGAR-TABELA-MESES UNTIL
               WRK-FS-ARQSEGTO                  EQUAL '10'.

           PERFORM 4000-PROCESSAR               UNTIL
               WRK-FS-SEGMOVTO                  EQUAL '10'.

           PERFORM 5000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT ARQSEGTO
                      SEGMOVTO
               OUTPUT ARQDRIVE
                      ARQDESPR
                      RELDESPR
                      RELDRIVE.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           INITIALIZE   TAB-MESES.

           CALL 'POOL7600'             USING WRK-DATA-HORA.

           MOVE WRK-DIA                TO CB3-DIA
                                          CB2-DIA.
           MOVE WRK-MES                TO CB3-MES
                                          CB2-MES.
           MOVE WRK-ANO                TO CB3-ANO
                                          CB2-ANO.
           MOVE WRK-HORA               TO CB3-HOR
                                          CB2-HOR.
           MOVE WRK-MIN                TO CB3-MIN
                                          CB2-MIN.
           MOVE WRK-SEG                TO CB3-SEG
                                          CB2-SEG.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-ARQSEGTO.

           PERFORM 1120-TESTAR-FS-SEGMOVTO.

           PERFORM 1130-TESTAR-FS-ARQDRIVE.

           PERFORM 1140-TESTAR-FS-ARQDESPR.

           PERFORM 1150-TESTAR-FS-RELDRIVE.

           PERFORM 1160-TESTAR-FS-RELDESPR.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1110-TESTAR-FS-ARQSEGTO         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQSEGTO         NOT EQUAL ZEROS
               MOVE 'ARQSEGTO'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-ARQSEGTO   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-TEXTO    TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1120-TESTAR-FS-SEGMOVTO         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SEGMOVTO         NOT EQUAL ZEROS
               MOVE 'SEGMOVTO'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-SEGMOVTO   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-TEXTO    TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1130-TESTAR-FS-ARQDRIVE         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQDRIVE         NOT EQUAL ZEROS
               MOVE 'ARQDRIVE'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-ARQDRIVE   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-TEXTO    TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1140-TESTAR-FS-ARQDESPR         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQDESPR         NOT EQUAL ZEROS
               MOVE 'ARQDESPR'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-ARQDESPR   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-TEXTO    TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1150-TESTAR-FS-RELDRIVE         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-RELDRIVE         NOT EQUAL ZEROS
               MOVE 'RELDRIVE'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-RELDRIVE   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-TEXTO    TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1160-TESTAR-FS-RELDESPR         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-RELDESPR         NOT EQUAL ZEROS
               MOVE 'RELDESPR'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-RELDESPR   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-TEXTO    TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1160-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-ARQSEGTO.

           IF  WRK-FS-ARQSEGTO         EQUAL '10'
               DISPLAY '***************** GFCT6865 ******************'
               DISPLAY '*                                           *'
               DISPLAY '*           ARQUIVO ARQSEGTO VAZIO          *'
               DISPLAY '*                                           *'
               DISPLAY '***************** GFCT6865 ******************'
           END-IF.

           PERFORM 2200-LER-SEGMOVTO.

           IF  WRK-FS-SEGMOVTO         EQUAL '10'
               DISPLAY '***************** GFCT6865 ******************'
               DISPLAY '*                                           *'
               DISPLAY '*           ARQUIVO SEGMOVTO VAZIO          *'
               DISPLAY '*                                           *'
               DISPLAY '*           PROCESSAMENTO ENCERRADO         *'
               DISPLAY '*                                           *'
               DISPLAY '***************** GFCT6865 ******************'
               PERFORM 5000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-LER-ARQSEGTO               SECTION.
      *----------------------------------------------------------------*

           READ ARQSEGTO.

           IF  WRK-FS-ARQSEGTO         EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-ARQSEGTO.
           MOVE GFCT0001-COD-GFCT OF ARQSEGTO
                                       TO WRK-COD-GFCT
           ADD 1                       TO ACU-LID-ARQSEGTO.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2200-LER-SEGMOVTO               SECTION.
      *----------------------------------------------------------------*

           READ SEGMOVTO.

           IF  WRK-FS-SEGMOVTO         EQUAL '10'
               GO TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-SEGMOVTO.

           MOVE GFCT0003-CESTA-SERVICO OF SEGMOVTO
                                       TO WRK-CESTA-SERV
           ADD 1                       TO ACU-LID-SEGMOVTO.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-CARREGAR-TABELA-MESES      SECTION.
      *----------------------------------------------------------------*

           IF WRK-COD-GFCT             GREATER  9999  OR
              WRK-COD-GFCT             LESS     1
              DISPLAY '***************** GFCT6865 ******************'
              DISPLAY '*                                           *'
              DISPLAY '*        ESTOURO NA TABELA DE MESES         *'
              DISPLAY '*                                           *'
              DISPLAY '***************** GFCT6865 ******************'
              MOVE WRK-MSG01           TO ERR-TEXTO
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF

           MOVE GFCT0001-PRAZO-ISENCAO(2:2) TO
                        TAB-MESES-DRIVE(WRK-COD-GFCT)
           MOVE WRK-COD-GFCT           TO
                        TAB-CESTA-SERV(WRK-COD-GFCT)

           PERFORM 2100-LER-ARQSEGTO.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  (GFCT0003-CPF-CNPJ OF SEGMOVTO NOT EQUAL WRK-CPF-ANT)
               MOVE GFCT0003-CPF-CNPJ  OF SEGMOVTO
                                       TO WRK-CPF-ANT
               IF  (WRK-CESTA-SERV EQUAL ZEROS) OR
                   (TAB-MESES-DRIVE(WRK-CESTA-SERV) EQUAL ZEROS)
                   PERFORM 4300-GRAVAR-ARQDESPR
                   PERFORM 4400-IMPRIMIR-RELDESPR
               ELSE
                   PERFORM 4100-GRAVAR-ARQDRIVE
                   PERFORM 4200-IMPRIMIR-RELDRIVE
               END-IF
           ELSE
               PERFORM 4300-GRAVAR-ARQDESPR
               PERFORM 4400-IMPRIMIR-RELDESPR
           END-IF.

           PERFORM 2200-LER-SEGMOVTO.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4050-MONTAR-DT-FIM-VALIDADE     SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT0003-DINIC-VALDD-SERVC OF SEGMOVTO
                                       TO WRK-DT-FIM-VALIDADE.

           MOVE WRK-DIA-FIM-VALIDAUX   TO WRK-DIA-FIM-VALIDADE
           MOVE WRK-MES-FIM-VALIDAUX   TO WRK-MES-FIM-VALIDADE
           MOVE WRK-ANO-FIM-VALIDAUX   TO WRK-ANO-FIM-VALIDADE

           MOVE TAB-MESES-DRIVE(WRK-CESTA-SERV)
                                       TO WRK-MESES-AUX.

           ADD WRK-MESES-AUX           TO WRK-MES-FIM-VALIDADE

           PERFORM UNTIL WRK-MES-FIM-VALIDADE LESS  12
                     OR  WRK-MES-FIM-VALIDADE EQUAL 12

               COMPUTE WRK-MES-FIM-VALIDADE = WRK-MES-FIM-VALIDADE - 12
               ADD 1                   TO WRK-ANO-FIM-VALIDADE
           END-PERFORM
           IF  WRK-MES-FIM-VALIDADE    EQUAL 02
               DIVIDE WRK-ANO-FIM-VALIDADE BY 4 GIVING WRK-RESULTADO
                                                REMAINDER WRK-RESTO
               IF  WRK-RESTO           EQUAL ZEROS
                   MOVE 29             TO WRK-DIA-FIM-VALIDADE
               ELSE
                   MOVE 28             TO WRK-DIA-FIM-VALIDADE
               END-IF
           ELSE
             IF WRK-MES-FIM-VALIDADE   EQUAL 01 OR 03 OR 05 OR
                                             07 OR 08 OR 10 OR 12
                MOVE 31                TO WRK-DIA-FIM-VALIDADE
             ELSE
                MOVE 30                TO WRK-DIA-FIM-VALIDADE
             END-IF
           END-IF.

      *----------------------------------------------------------------*
       4050-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4100-GRAVAR-ARQDRIVE            SECTION.
      *----------------------------------------------------------------*

           PERFORM 4050-MONTAR-DT-FIM-VALIDADE.

           MOVE GFCT0003-REGISTRO OF SEGMOVTO TO GFCT0003-REGISTRO
                                                 OF ARQDRIVE.

           MOVE TAB-MESES-DRIVE(WRK-CESTA-SERV)
                                           TO GFCT0003-PRAZO-ISENCAO
                                           OF ARQDRIVE
                                            WRK-MESES-DRIVE.

           MOVE WRK-ANO-FIM-VALIDADE       TO WRK-ANO-FIM-VALSAI
           MOVE WRK-MES-FIM-VALIDADE       TO WRK-MES-FIM-VALSAI
           MOVE WRK-DIA-FIM-VALIDADE       TO WRK-DIA-FIM-VALSAI

           MOVE WRK-DT-FIM-VALSAI          TO GFCT0003-DFIM-TST-DRIVE
                                           OF ARQDRIVE.

           MOVE GFCT0003-DINIC-VALDD-SERVC OF SEGMOVTO
                                           TO WRK-DT-INCLUSAO.

           MOVE WRK-ANO-INCLUSAO           TO WRK-ANO-INCLUSAI
           MOVE WRK-MES-INCLUSAO           TO WRK-MES-INCLUSAI
           MOVE WRK-DIA-INCLUSAO           TO WRK-DIA-INCLUSAI

           MOVE WRK-DT-INCLUSAI            TO GFCT0003-DINIC-TST-DRIVE
                                           OF ARQDRIVE.

           MOVE WRK-GRAVACAO             TO WRK-OPERACAO.

           WRITE GFCT0003-REGISTRO       OF ARQDRIVE.

           PERFORM 1130-TESTAR-FS-ARQDRIVE.

           ADD 1                         TO ACU-GRA-ARQDRIVE.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4200-IMPRIMIR-RELDRIVE          SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LINHAS-RELDRIVE     GREATER 60
               PERFORM 4210-IMPRIMIR-WRK-C-RELDRIVE
           END-IF.

           MOVE GFCT0003-CPF-CNPJ      OF SEGMOVTO
                                       TO LD1-CPF-NUM.
           MOVE GFCT0003-FILIAL        OF SEGMOVTO
                                       TO LD1-CPF-FIL.
           MOVE GFCT0003-CONTROLE      OF SEGMOVTO
                                       TO LD1-CPF-CTRL.
           MOVE GFCT0003-CJUNC-DEPDC   OF SEGMOVTO
                                       TO LD1-AGENCIA.
           MOVE GFCT0003-RAZAO         OF SEGMOVTO
                                       TO LD1-RAZAO.
           MOVE GFCT0003-CCTA-CORR     OF SEGMOVTO
                                       TO LD1-CONTA.
           MOVE WRK-CESTA-SERV
                                       TO LD1-CESTA-SERV.
           MOVE GFCT0003-DINIC-VALDD-SERVC OF SEGMOVTO
                                       TO WRK-DT-INCLUSAO.
           MOVE WRK-ANO-INCLUSAO       TO LD1-ANO-INCLUSAO.
           MOVE WRK-MES-INCLUSAO       TO LD1-MES-INCLUSAO.
           MOVE WRK-DIA-INCLUSAO       TO LD1-DIA-INCLUSAO.
           MOVE WRK-MESES-DRIVE        TO LD1-MESES-DRIVE.
           MOVE WRK-ANO-FIM-VALIDADE   TO LD1-ANO-FIM-VALIDADE.
           MOVE WRK-MES-FIM-VALIDADE   TO LD1-MES-FIM-VALIDADE.
           MOVE WRK-DIA-FIM-VALIDADE   TO LD1-DIA-FIM-VALIDADE.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE FD-REG-RELDRIVE       FROM WRK-LIN-DET1.

           PERFORM 1150-TESTAR-FS-RELDRIVE.

           ADD 1                       TO ACU-LINHAS-RELDRIVE.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4210-IMPRIMIR-WRK-C-RELDRIVE    SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO ACU-PAG-RELDRIVE.

           MOVE ACU-PAG-RELDRIVE       TO CB1-PAG.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE FD-REG-RELDRIVE       FROM WRK-C1.

           PERFORM 1150-TESTAR-FS-RELDRIVE.

           WRITE FD-REG-RELDRIVE       FROM WRK-C3.

           PERFORM 1150-TESTAR-FS-RELDRIVE.

           WRITE FD-REG-RELDRIVE       FROM WRK-C4.

           PERFORM 1150-TESTAR-FS-RELDRIVE.

           WRITE FD-REG-RELDRIVE       FROM WRK-C5.

           PERFORM 1150-TESTAR-FS-RELDRIVE.

           MOVE 6                      TO ACU-LINHAS-RELDRIVE.

      *----------------------------------------------------------------*
       4210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4300-GRAVAR-ARQDESPR            SECTION.
      *----------------------------------------------------------------*

           PERFORM 4050-MONTAR-DT-FIM-VALIDADE.

           MOVE GFCT0003-REGISTRO OF SEGMOVTO TO GFCT0003-REGISTRO
                                                 OF ARQDESPR.

           MOVE WRK-ANO-FIM-VALIDADE       TO WRK-ANO-FIM-VALSAI
           MOVE WRK-MES-FIM-VALIDADE       TO WRK-MES-FIM-VALSAI
           MOVE WRK-DIA-FIM-VALIDADE       TO WRK-DIA-FIM-VALSAI

           MOVE WRK-DT-FIM-VALSAI          TO GFCT0003-DFIM-TST-DRIVE
                                           OF ARQDESPR.

           MOVE GFCT0003-DINIC-VALDD-SERVC OF SEGMOVTO
                                           TO WRK-DT-INCLUSAO.

           MOVE WRK-ANO-INCLUSAO           TO WRK-ANO-INCLUSAI
           MOVE WRK-MES-INCLUSAO           TO WRK-MES-INCLUSAI
           MOVE WRK-DIA-INCLUSAO           TO WRK-DIA-INCLUSAI

           MOVE WRK-DT-INCLUSAI            TO GFCT0003-DINIC-TST-DRIVE
                                           OF ARQDESPR.

           MOVE TAB-MESES-DRIVE(WRK-CESTA-SERV)
                                         TO GFCT0003-PRAZO-ISENCAO
                                            OF ARQDESPR
                                            WRK-MESES-DRIVE.

           MOVE WRK-GRAVACAO              TO WRK-OPERACAO.

           WRITE GFCT0003-REGISTRO OF ARQDESPR.

           PERFORM 1140-TESTAR-FS-ARQDESPR.

           ADD 1                          TO ACU-GRA-ARQDESPR.

      *----------------------------------------------------------------*
       4300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4400-IMPRIMIR-RELDESPR          SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LINHAS-RELDESPR     GREATER 60
               PERFORM 4410-IMPRIMIR-WRK-C-RELDESPR
           END-IF.

           MOVE GFCT0003-CPF-CNPJ      OF SEGMOVTO
                                       TO LD1-CPF-NUM.
           MOVE GFCT0003-FILIAL        OF SEGMOVTO
                                       TO LD1-CPF-FIL.
           MOVE GFCT0003-CONTROLE      OF SEGMOVTO
                                       TO LD1-CPF-CTRL.
           MOVE GFCT0003-CJUNC-DEPDC   OF SEGMOVTO
                                       TO LD1-AGENCIA.
           MOVE GFCT0003-RAZAO         OF SEGMOVTO
                                       TO LD1-RAZAO.
           MOVE GFCT0003-CCTA-CORR     OF SEGMOVTO
                                       TO LD1-CONTA.
           MOVE WRK-CESTA-SERV
                                       TO LD1-CESTA-SERV.
           MOVE GFCT0003-DINIC-VALDD-SERVC OF SEGMOVTO
                                       TO WRK-DT-INCLUSAO.
           MOVE WRK-ANO-INCLUSAO       TO LD1-ANO-INCLUSAO.
           MOVE WRK-MES-INCLUSAO       TO LD1-MES-INCLUSAO.
           MOVE WRK-DIA-INCLUSAO       TO LD1-DIA-INCLUSAO.
           MOVE WRK-MESES-DRIVE        TO LD1-MESES-DRIVE.
           MOVE WRK-ANO-FIM-VALIDADE   TO LD1-ANO-FIM-VALIDADE.
           MOVE WRK-MES-FIM-VALIDADE   TO LD1-MES-FIM-VALIDADE.
           MOVE WRK-DIA-FIM-VALIDADE   TO LD1-DIA-FIM-VALIDADE.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE FD-REG-RELDESPR       FROM WRK-LIN-DET1.

           PERFORM 1160-TESTAR-FS-RELDESPR.

           ADD 1                       TO ACU-LINHAS-RELDESPR.

      *----------------------------------------------------------------*
       4400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4410-IMPRIMIR-WRK-C-RELDESPR    SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO ACU-PAG-RELDESPR.

           MOVE ACU-PAG-RELDESPR       TO CB1-PAG.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE FD-REG-RELDESPR       FROM WRK-C1.

           PERFORM 1160-TESTAR-FS-RELDESPR.

           WRITE FD-REG-RELDESPR       FROM WRK-C2.

           PERFORM 1160-TESTAR-FS-RELDESPR.

           WRITE FD-REG-RELDESPR       FROM WRK-C4.

           PERFORM 1160-TESTAR-FS-RELDESPR.

           WRITE FD-REG-RELDESPR       FROM WRK-C5.

           PERFORM 1160-TESTAR-FS-RELDESPR.

           MOVE 6                      TO ACU-LINHAS-RELDESPR.

      *----------------------------------------------------------------*
       4410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       5000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 5100-EMITIR-TOTAIS-RELDRIVE.

           PERFORM 5200-EMITIR-TOTAIS-RELDESPR.

           PERFORM 5300-EMITIR-ESTATISTICAS.

           CLOSE ARQSEGTO
                 SEGMOVTO
                 ARQDRIVE
                 ARQDESPR
                 RELDESPR
                 RELDRIVE.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           GOBACK.

      *----------------------------------------------------------------*
       5000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5100-EMITIR-TOTAIS-RELDRIVE     SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           IF  ACU-LINHAS-RELDRIVE     GREATER 50
               ADD 1                   TO ACU-PAG-RELDRIVE
               MOVE ACU-PAG-RELDRIVE   TO CB1-PAG
               WRITE FD-REG-RELDRIVE   FROM WRK-C1
               PERFORM 1150-TESTAR-FS-RELDRIVE
               WRITE FD-REG-RELDRIVE   FROM WRK-C3
               PERFORM 1150-TESTAR-FS-RELDRIVE
           END-IF.

           MOVE ACU-LID-ARQSEGTO       TO WRK-TOT-LARQSEG.
           MOVE ACU-LID-SEGMOVTO       TO WRK-TOT-LSEGMOV.
           MOVE ACU-GRA-ARQDRIVE       TO WRK-TOT-GARQDRIV.
           MOVE ACU-GRA-ARQDESPR       TO WRK-TOT-GARQDE.
           MOVE ACU-DES-ARQSEGTO       TO WRK-TOT-DARQSEG.

           WRITE FD-REG-RELDRIVE       FROM WRK-C5.

           PERFORM 1150-TESTAR-FS-RELDRIVE.

           WRITE FD-REG-RELDRIVE       FROM WRK-C5.

           PERFORM 1150-TESTAR-FS-RELDRIVE.

           WRITE FD-REG-RELDRIVE       FROM WRK-C5.

           PERFORM 1150-TESTAR-FS-RELDRIVE.

           WRITE FD-REG-RELDRIVE       FROM WRK-LIN-TOT1.

           PERFORM 1150-TESTAR-FS-RELDRIVE.

           WRITE FD-REG-RELDRIVE       FROM WRK-LIN-TOT1A.

           PERFORM 1150-TESTAR-FS-RELDRIVE.

           WRITE FD-REG-RELDRIVE       FROM WRK-LIN-TOT2.

           PERFORM 1150-TESTAR-FS-RELDRIVE.

           WRITE FD-REG-RELDRIVE       FROM WRK-LIN-TOT2A.

           PERFORM 1150-TESTAR-FS-RELDRIVE.

           WRITE FD-REG-RELDRIVE       FROM WRK-LIN-TOT3.

           PERFORM 1150-TESTAR-FS-RELDRIVE.

      *----------------------------------------------------------------*
       5100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       5200-EMITIR-TOTAIS-RELDESPR     SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           IF  ACU-LINHAS-RELDESPR     GREATER 50
               ADD 1                   TO ACU-PAG-RELDESPR
               MOVE ACU-PAG-RELDESPR   TO CB1-PAG
               WRITE FD-REG-RELDESPR   FROM WRK-C1
               PERFORM 1160-TESTAR-FS-RELDESPR
               WRITE FD-REG-RELDESPR   FROM WRK-C2
               PERFORM 1160-TESTAR-FS-RELDESPR
           END-IF.

           WRITE FD-REG-RELDESPR       FROM WRK-C5.

           PERFORM 1160-TESTAR-FS-RELDESPR.

           WRITE FD-REG-RELDESPR       FROM WRK-C5.

           PERFORM 1160-TESTAR-FS-RELDESPR.

           WRITE FD-REG-RELDESPR       FROM WRK-C5.

           PERFORM 1160-TESTAR-FS-RELDESPR.

           WRITE FD-REG-RELDESPR       FROM WRK-LIN-TOT1.

           PERFORM 1160-TESTAR-FS-RELDESPR.

           WRITE FD-REG-RELDESPR       FROM WRK-LIN-TOT1A.

           PERFORM 1160-TESTAR-FS-RELDESPR.

           WRITE FD-REG-RELDESPR       FROM WRK-LIN-TOT2.

           PERFORM 1160-TESTAR-FS-RELDESPR.

           WRITE FD-REG-RELDESPR       FROM WRK-LIN-TOT2A.

           PERFORM 1160-TESTAR-FS-RELDESPR.

           WRITE FD-REG-RELDESPR       FROM WRK-LIN-TOT3.

           PERFORM 1160-TESTAR-FS-RELDESPR.

      *----------------------------------------------------------------*
       5200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5300-EMITIR-ESTATISTICAS        SECTION.
      *----------------------------------------------------------------*

           DISPLAY '***************** GFCT6865 ******************'.
           DISPLAY '*                                           *'.

           MOVE ACU-LID-ARQSEGTO       TO WRK-MASCARA

           DISPLAY '* TOTAL LIDOS    ARQSEGTO   : ' WRK-MASCARA ' *'.

           MOVE ACU-LID-SEGMOVTO       TO WRK-MASCARA

           DISPLAY '* TOTAL LIDOS    SEGMOVTO   : ' WRK-MASCARA ' *'.

           MOVE ACU-GRA-ARQDRIVE       TO WRK-MASCARA

           DISPLAY '* TOTAL GRAVADOS ARQDRIVE   : ' WRK-MASCARA ' *'.

           MOVE ACU-GRA-ARQDESPR       TO WRK-MASCARA

           DISPLAY '* TOTAL GRAVADOS ARQDESPR   : ' WRK-MASCARA ' *'.
           DISPLAY '*                                           *'.
           DISPLAY '***************** GFCT6865 ******************'.

      *----------------------------------------------------------------*
       5300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'APL'                  TO ERR-TIPO-ACESSO.
           MOVE 'GFCT6865'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
