      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*

       PROGRAM-ID. GFCT4468.

      *================================================================*
      *                       S O N D A   I T                          *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA.........: GFCT4468                                 *
      *    PROGRAMADOR......: FABRICA SOFTWARE        - SONDA PROCWORK *
      *                                                                *
      *    ANALISTA.........: CIBELE BARBOSA                           *
      *    LIDER PROJETO....: RODRIGO PRADO                            *
      *    GERENCIA / EQUIPE: GERENCIA-E / EQUIPE-1   - SONDA PROCWORK *
      *                                                                *
      *    GRUPO NEGOCIO....: DDS-GP50                                 *
      *                                                                *
      *    DATA.............: ABR/2014                                 *
      *                                                                *
      *    PROJETO..........:                                          *
      *                                                                *
      *    OBJETIVO.........: RELATORIO DE BONIFICACOES POR DESCONTO   *
      *                       PROGRESSIVO.                             *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                            INCLUDE/BOOK            *
      *        ARQHISTE                          GFCTWBBF              *
      *        RELATORI                          --------              *
      *                                                                *
      *    MODULOS CHAMADOS.:                                          *
      *      BRAD7100 - TRATAR ERRO                                    *
      *      BRAD7600 - OBTER DATA DO SISTEMA                          *
      *                                                                *
      *================================================================*

      *===============================================================*
       ENVIRONMENT DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       CONFIGURATION SECTION.
      *---------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *---------------------------------------------------------------*
       INPUT-OUTPUT SECTION.
      *---------------------------------------------------------------*

       FILE-CONTROL.

           SELECT ARQHISTE ASSIGN TO UT-S-ARQHISTE
                      FILE STATUS IS WRK-FS-ARQHISTE.

           SELECT RELATORI ASSIGN TO UT-S-RELATORI
                      FILE STATUS IS WRK-FS-RELATORI.

      *===============================================================*
       DATA DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *   INPUT :     ORG. SEQUENCIAL   -   LRECL = 140               *
      *---------------------------------------------------------------*

       FD  ARQHISTE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-ARQHISTE             PIC X(140).

      *---------------------------------------------------------------*
      *   OUTPUT:     ORG. SEQUENCIAL   -   LRECL = 133               *
      *---------------------------------------------------------------*

       FD  RELATORI
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-RELATORI             PIC X(133).

      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
       77  FILLER  PIC X(32)  VALUE  '*  INICIO DA WORKING GFCT4468  *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*   AREAS AUXILIARES           *'.
      *---------------------------------------------------------------*

       01  WRK-HORA-REL.
           05  WRK-HOR-REL             PIC 9(02)           VALUE ZEROS.
           05  FILLER                  PIC X(01)           VALUE ':'.
           05  WRK-MIN-REL             PIC 9(02)           VALUE ZEROS.
           05  FILLER                  PIC X(01)           VALUE ':'.
           05  WRK-SEG-REL             PIC 9(02)           VALUE ZEROS.

       01  WRK-DATA-DMA.
           05  WRK-DIA-DMA             PIC 9(02)           VALUE ZEROS.
           05  FILLER                  PIC X(01)           VALUE '/'.
           05  WRK-MES-DMA             PIC 9(02)           VALUE ZEROS.
           05  FILLER                  PIC X(01)           VALUE '/'.
           05  WRK-ANO-DMA             PIC 9(04)           VALUE ZEROS.

       01  WRK-DATA-PONTO.
           05  WRK-DIA-PONTO           PIC 9(02)           VALUE ZEROS.
           05  FILLER                  PIC X(01)           VALUE SPACES.
           05  WRK-MES-PONTO           PIC 9(02)           VALUE ZEROS.
           05  FILLER                  PIC X(01)           VALUE SPACES.
           05  WRK-ANO-PONTO           PIC 9(04)           VALUE ZEROS.

       01 WRK-VALOR.
          05 WRK-MASC-VALOR          PIC -------------9,99 VALUE ZEROS.

       01 WRK-QUANTIDADE.
          05  FILLER                 PIC X(010) VALUE SPACES.
          05  WRK-MASC-QUANTIDADE    PIC -----9 VALUE ZEROS.

       01 WRK-VALOR-COBR.
          05 WRK-MASC-VALOR-COBR     PIC ---------9,99 VALUE ZEROS.

       01 WRK-PERC-DESC.
          05  FILLER                 PIC X(001) VALUE SPACES.
          05  WRK-MASC-PERC-DESC     PIC ----9,9999 VALUE ZEROS.
          05  FILLER                 PIC X(001) VALUE '%'.

       01 WRK-QFRANQ-DESC.
          05 WRK-MASC-QFRANQ         PIC --------999 VALUE ZEROS.

       01  WRK-CONSE-DADO-S9-15       PIC S9(15)          VALUE ZEROS.
       01  FILLER                     REDEFINES WRK-CONSE-DADO-S9-15.
           05  FILLER                 PIC  9(10).
           05  WRK-CONSE-DADO-S9-05   PIC S9(05).

       01  WRK-AUXILIARES.
           05  WRK-S-909               PIC +9(009) VALUE ZEROS.
           05  WRK-FILLER              REDEFINES WRK-S-909.
             10 FILLER                 PIC X(01).
             10 WRK-909                PIC 9(09).
           05  WRK-S-905               PIC +9(005) VALUE ZEROS.
           05  WRK-FILLER              REDEFINES WRK-S-905.
             10 FILLER                 PIC X(01).
             10 WRK-905                PIC 9(05).
           05  WRK-S-913               PIC +9(013) VALUE ZEROS.
           05  WRK-FILLER              REDEFINES WRK-S-913.
             10 FILLER                 PIC X(01).
             10 WRK-913                PIC 9(13).
           05  WRK-S-903               PIC +9(003) VALUE ZEROS.
           05  WRK-FILLER              REDEFINES WRK-S-903.
             10 FILLER                 PIC X(01).
             10 WRK-903                PIC 9(03).

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*   AREAS DE FILE STATUS       *'.
      *---------------------------------------------------------------*

       77  WRK-BATCH                   PIC X(08) VALUE 'BATCH   '.

       77  WRK-FS-ARQHISTE             PIC X(02) VALUE SPACES.
       77  WRK-FS-RELATORI             PIC X(02) VALUE SPACES.

       77  WRK-OPERACAO                PIC X(13) VALUE SPACES.
       77  WRK-ABERTURA                PIC X(13) VALUE 'NA ABERTURA'.
       77  WRK-LEITURA                 PIC X(13) VALUE 'NA LEITURA'.
       77  WRK-GRAVACAO                PIC X(13) VALUE 'NA GRAVACAO'.
       77  WRK-FECHAMENTO              PIC X(13) VALUE 'NO FECHAMENTO'.

      *---------------------------------------------------------------*
       77  FILLER  PIC X(32)  VALUE  '* ACUMULADORES                 *'.
      *---------------------------------------------------------------*

       77  ACU-LIDOS-ARQHISTE          PIC 9(09) COMP-3    VALUE ZEROS.
       77  ACU-GRAVA-RELATORI          PIC 9(09) COMP-3    VALUE ZEROS.

       77  ACU-PAGINAS                 PIC 9(10) COMP-3    VALUE ZEROS.
       77  ACU-LINHAS                  PIC 9(03) COMP-3    VALUE 99.

      *---------------------------------------------------------------*
       77  FILLER  PIC X(32)  VALUE  '* MASCARAS DE EDICAO DE TOTAIS *'.
      *---------------------------------------------------------------*

       77  WRK-MASK-LIDOS-ARQHISTE     PIC Z.ZZZ.ZZZ.ZZ9   VALUE ZEROS.
       77  WRK-MASK-GRAVA-RELATORI     PIC Z.ZZZ.ZZZ.ZZ9   VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*   AREAS BRAD7600             *'.
      *---------------------------------------------------------------*

       01  WRK-DATA-HORA-7600.
           05  WRK-DT-JULIANA          PIC 9(05) COMP-3    VALUE ZEROS.
           05  WRK-DT-AAMMDD           PIC 9(07) COMP-3    VALUE ZEROS.
           05  WRK-DT-AAAAMMDD         PIC 9(09) COMP-3    VALUE ZEROS.
           05  WRK-TI-HHMMSS           PIC 9(07) COMP-3    VALUE ZEROS.
           05  WRK-TI-HHMMSSMMMMMM     PIC 9(13) COMP-3    VALUE ZEROS.
           05  WRK-TIMESTAMP           PIC X(20)           VALUE SPACES.

       01  WRK-DT-SISTEMA              PIC 9(09)           VALUE ZEROS.
       01  FILLER      REDEFINES       WRK-DT-SISTEMA.
           05  FILLER                  PIC X(01).
           05  WRK-ANO-SISTEMA         PIC 9(04).
           05  WRK-MES-SISTEMA         PIC 9(02).
           05  WRK-DIA-SISTEMA         PIC 9(02).

       01  WRK-HR-SISTEMA              PIC 9(07)           VALUE ZEROS.
       01  FILLER      REDEFINES       WRK-HR-SISTEMA.
           05  FILLER                  PIC X(01).
           05  WRK-HOR-SISTEMA         PIC 9(02).
           05  WRK-MIN-SISTEMA         PIC 9(02).
           05  WRK-SEG-SISTEMA         PIC 9(02).

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*   AREAS LAYOUT ARQHISTE      *'.
      *---------------------------------------------------------------*

       COPY 'GFCTWBBF'.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*  LAYOUT RELATORI             *'.
      *---------------------------------------------------------------*

       01  CABEC1-REL1.
           05  FILLER                  PIC X(001) VALUE '1'.
           05  CB01-REL1-DATA          PIC X(010) VALUE SPACES.
           05  FILLER                  PIC X(044) VALUE SPACES.
           05  FILLER                  PIC X(069) VALUE
               'B A N C O  B R A D E S C O  S / A'.
           05  CB01-REL1-HORA          PIC X(008) VALUE SPACES.

       01  CABEC2-REL1.
           05  FILLER                  PIC X(001) VALUE ' '.
           05  FILLER                  PIC X(044) VALUE 'GFCT4468'.
           05  FILLER                  PIC X(071) VALUE
               'GFCT - GESTAO, FLEXIBILIZACAO E COBRANCA DE TARIFAS'.
           05  FILLER                  PIC X(004) VALUE 'PAG.'.
           05  CB02-REL1-PAGINA        PIC ZZZZ.ZZZ.ZZ9 VALUE ZEROS.

       01  CABEC3-REL1.
           05  FILLER                  PIC X(001) VALUE ' '.
           05  FILLER                  PIC X(050) VALUE SPACES.
           05  FILLER                  PIC X(081) VALUE
               ' BONIFICACOES POR DESCONTO PROGRESSIVO  '.

       01  CABEC4-REL1.
           05  FILLER                  PIC X(001) VALUE ' '.
           05  FILLER                  PIC X(132) VALUE ALL '-'.

       01  CABEC5-REL1.
           05  FILLER                  PIC X(001) VALUE ' '.
           05  FILLER                  PIC X(044) VALUE
               'CPF/CNPJ           AGENC CONTA         CART '.
           05  FILLER                  PIC X(045) VALUE
               ' TARIF DT EVENTO  DT ABERT   VL SIGB ANTERIOR'.
           05  FILLER                  PIC X(041) VALUE
               '     VL SIGB ATUAL  TP FX  SEQ FRANQ/DESC'.

       01  CABEC6-REL1.
           05  FILLER                  PIC X(133) VALUE SPACES.

       01  LINDET1-REL1.
           05  FILLER                  PIC X(001) VALUE ' '.
ST25X6*      05  LD1-REL1-CPF-PRINC      PIC 9(009) VALUE ZEROS.
ST25X6       05  LD1-REL1-CPF-PRINC      PIC X(009) VALUE SPACES.
           05  FILLER                  PIC X(001) VALUE '/'.
ST25X6*      05  LD1-REL1-CPF-FILIAL     PIC 9(005) VALUE ZEROS.
ST25X6       05  LD1-REL1-CPF-FILIAL     PIC X(005) VALUE SPACES.
           05  FILLER                  PIC X(001) VALUE '-'.
ST25X6*      05  LD1-REL1-CPF-CTRL       PIC 9(002) VALUE ZEROS.
ST25X6       05  LD1-REL1-CPF-CTRL       PIC X(002) VALUE SPACES.
           05  FILLER                  PIC X(001) VALUE SPACES.
           05  LD1-REL1-NRO-AGE        PIC 9(005) VALUE ZEROS.
           05  FILLER                  PIC X(001) VALUE SPACES.
           05  LD1-REL1-NRO-CTA        PIC 9(013) VALUE ZEROS.
           05  FILLER                  PIC X(001) VALUE SPACES.
           05  LD1-REL1-CARTEIRA       PIC 9(005) VALUE ZEROS.
           05  FILLER                  PIC X(001) VALUE SPACES.
           05  LD1-REL1-TARIFA         PIC 9(005) VALUE ZEROS.
           05  FILLER                  PIC X(001) VALUE SPACES.
           05  LD1-REL1-DT-EVENTO      PIC X(010) VALUE SPACES.
           05  FILLER                  PIC X(001) VALUE SPACES.
           05  LD1-REL1-DT-ABERTURA    PIC X(010) VALUE SPACES.
           05  FILLER                  PIC X(001) VALUE SPACES.
           05  LD1-REL1-VL-SIGB-ANT    PIC X(017) VALUE SPACES.
           05  FILLER                  PIC X(001) VALUE SPACES.
           05  LD1-REL1-VL-SIGB-ATU    PIC X(017) VALUE SPACES.
           05  FILLER                  PIC X(001) VALUE SPACES.
           05  LD1-REL1-TIPO-FAIXA     PIC X(006) VALUE SPACES.
           05  FILLER                  PIC X(001) VALUE SPACES.
           05  LD1-REL1-SEQUENCIA      PIC 9(003) VALUE ZEROS.
           05  FILLER                  PIC X(001) VALUE SPACES.
           05  LD1-REL1-FRANQUIA       PIC X(014) VALUE SPACES.


       01  LINTOT1-REL1.
           05  FILLER                  PIC X(001) VALUE ' '.
           05  FILLER                  PIC X(039) VALUE SPACES.
           05  FILLER                  PIC X(051) VALUE
               'TOTAL DE BONIFICACOES......................... ='.
           05  LT1-REL1-TOTAIS         PIC Z.ZZZ.ZZZ.ZZ9   VALUE ZEROS.
      *---------------------------------------------------------------*
       01  FILLER    PIC X(32)   VALUE '*   AREAS TRATAMENTO ERRO    *'.
      *---------------------------------------------------------------*

       COPY 'I#BRAD7C'.

       01  WRK-TEXTO.
           05  FILLER                  PIC X(05) VALUE 'ERRO'.
           05  WRK-OPERACAO-TXT        PIC X(13) VALUE SPACES.
           05  FILLER                  PIC X(12) VALUE ' DO ARQUIVO '.
           05  WRK-ARQ-TXT             PIC X(08) VALUE SPACES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
       01  FILLER  PIC X(32)  VALUE  '*  FIM  DA WORKING   GFCT4468  *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*

      *===============================================================*
       PROCEDURE DIVISION.
      *===============================================================*

      *===============================================================*
      *    ROTINA INICIAR
      *===============================================================*
       0000-INICIAR SECTION.
      *---------------------------------------------------------------*

           OPEN    INPUT   ARQHISTE
                   OUTPUT  RELATORI

           MOVE    WRK-ABERTURA        TO  WRK-OPERACAO.
           PERFORM 1000-TESTAR-FILE-STATUS.

           PERFORM 2000-ROTINA-INICIAL.

           PERFORM 3000-PROCESSA
             UNTIL WRK-FS-ARQHISTE EQUAL '10'.

           PERFORM 4000-ROTINA-FINAL.


      *===============*
      * LABEL FINALIZAR
      *===============*
       0000-FINALIZAR.
      *---------------*

           CLOSE   ARQHISTE
                   RELATORI.

           MOVE    WRK-FECHAMENTO      TO   WRK-OPERACAO.
           PERFORM 1000-TESTAR-FILE-STATUS.

           STOP    RUN.

      *---------------------------------------------------------------*
       0000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FILE-STATUS
      *===============================================================*
       1000-TESTAR-FILE-STATUS SECTION.
      *---------------------------------------------------------------*

           PERFORM 1100-TESTAR-FS-ARQHISTE.

           PERFORM 1200-TESTAR-FS-RELATORI.

      *---------------------------------------------------------------*
       1000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-ARQHISTE
      *===============================================================*
       1100-TESTAR-FS-ARQHISTE SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-ARQHISTE NOT EQUAL '00'
               DISPLAY '************** GFCT4468 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*              ARQHISTE             *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQHISTE
                                                  '         *'
               DISPLAY '*         PROGRAMA ENCERRADO        *'
               DISPLAY '************** GFCT4468 *************'
               MOVE WRK-OPERACAO       TO  WRK-OPERACAO-TXT
               MOVE 'ARQHISTE'         TO  WRK-ARQ-TXT
               MOVE WRK-TEXTO          TO  ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-RELATORI
      *===============================================================*
       1200-TESTAR-FS-RELATORI   SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RELATORI     NOT EQUAL '00'
               DISPLAY '************** GFCT4468 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*              RELATORI             *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-RELATORI
                                                  '         *'
               DISPLAY '*         PROGRAMA ENCERRADO        *'
               DISPLAY '************** GFCT4468 *************'
               MOVE WRK-OPERACAO       TO  WRK-OPERACAO-TXT
               MOVE 'RELATORI'         TO  WRK-ARQ-TXT
               MOVE WRK-TEXTO          TO  ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA ROTINA-INICIAL
      *===============================================================*
       2000-ROTINA-INICIAL SECTION.
      *---------------------------------------------------------------*

      *--> VALORIZAR CAMPOS DO CABECALHO DO RELATORIO

           CALL 'BRAD7600'          USING  WRK-DATA-HORA-7600.
           MOVE WRK-TI-HHMMSS          TO  WRK-HR-SISTEMA.
           MOVE WRK-HOR-SISTEMA        TO  WRK-HOR-REL.
           MOVE WRK-MIN-SISTEMA        TO  WRK-MIN-REL.
           MOVE WRK-SEG-SISTEMA        TO  WRK-SEG-REL.
           MOVE WRK-HORA-REL           TO  CB01-REL1-HORA.

           MOVE WRK-DT-AAAAMMDD        TO  WRK-DT-SISTEMA
           MOVE WRK-ANO-SISTEMA        TO  WRK-ANO-DMA
           MOVE WRK-MES-SISTEMA        TO  WRK-MES-DMA
           MOVE WRK-DIA-SISTEMA        TO  WRK-DIA-DMA
           MOVE WRK-DATA-DMA           TO  CB01-REL1-DATA.

           PERFORM 2200-LER-ARQHISTE.

           IF  ACU-LIDOS-ARQHISTE      EQUAL   ZEROS
               DISPLAY '************* GFCT4468 *************'
               DISPLAY '*                                  *'
               DISPLAY '*   ARQUIVO ARQHISTE ESTA VAZIO    *'
               DISPLAY '*                                  *'
               DISPLAY '************* GFCT4468 *************'
           END-IF.

      *---------------------------------------------------------------*
       2000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA LER-ARQHISTE
      *===============================================================*
       2200-LER-ARQHISTE SECTION.
      *---------------------------------------------------------------*

           READ ARQHISTE                INTO GFCTWBBF-REG.

           IF  WRK-FS-ARQHISTE      EQUAL  '10'
               GO                      TO  2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO  WRK-OPERACAO.
           PERFORM 1100-TESTAR-FS-ARQHISTE.

           ADD  1                      TO  ACU-LIDOS-ARQHISTE.

      *---------------------------------------------------------------*
       2200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA PROCESSA
      *===============================================================*
       3000-PROCESSA SECTION.
      *---------------------------------------------------------------*

           PERFORM 3100-IMPRIMIR-RELATORIO

           PERFORM 2200-LER-ARQHISTE.

      *---------------------------------------------------------------*
       3000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA IMPRIMIR-RELATORIO
      *===============================================================*
       3100-IMPRIMIR-RELATORIO     SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO.

           IF  ACU-LINHAS              GREATER 59
               PERFORM 3200-IMPRIMIR-CABECALHO
           END-IF.

ST25X9*    MOVE WBBF-CPF-CNPJ               TO WRK-S-909.
ST25X9*    MOVE WRK-909                     TO LD1-REL1-CPF-PRINC.
ST25X9     MOVE WBBF-CPF-CNPJ               TO LD1-REL1-CPF-PRINC.

ST25X9*    MOVE WBBF-FILIAL                 TO WRK-S-905.
ST25X9*    MOVE WRK-905                     TO LD1-REL1-CPF-FILIAL
ST25X9     MOVE WBBF-FILIAL                 TO LD1-REL1-CPF-FILIAL

           MOVE WBBF-CONTROLE               TO LD1-REL1-CPF-CTRL.

           MOVE WBBF-CAG-DSTNO-MOVTO        TO WRK-S-905.
           MOVE WRK-905                     TO LD1-REL1-NRO-AGE.

           MOVE WBBF-CCTA-DSTNO-MOVTO       TO WRK-S-913.
           MOVE WRK-913                     TO LD1-REL1-NRO-CTA.

           MOVE WBBF-23-CCART-ANLSE-GERC    TO WRK-S-905.
           MOVE WRK-905                     TO LD1-REL1-CARTEIRA.

           MOVE WBBF-CSERVC-TARIF           TO WRK-S-905.
           MOVE WRK-905                     TO LD1-REL1-TARIFA.

           MOVE WBBF-DOCOR-EVNTO            TO WRK-DATA-PONTO.
           MOVE WRK-DIA-PONTO               TO WRK-DIA-DMA.
           MOVE WRK-MES-PONTO               TO WRK-MES-DMA.
           MOVE WRK-ANO-PONTO               TO WRK-ANO-DMA.
           MOVE WRK-DATA-DMA                TO LD1-REL1-DT-EVENTO.

           MOVE WBBF-DATA-ABERTURA          TO WRK-DT-SISTEMA.

           MOVE WRK-ANO-SISTEMA             TO WRK-ANO-DMA.

           MOVE WRK-MES-SISTEMA             TO WRK-MES-DMA.

           MOVE WRK-DIA-SISTEMA             TO WRK-DIA-DMA.

           MOVE WRK-DATA-DMA                TO LD1-REL1-DT-ABERTURA.

           MOVE WBBF-23-CSEQ-BONIF-SERVC    TO WRK-S-903.
           MOVE WRK-903                     TO LD1-REL1-SEQUENCIA.

           IF WBBF-23-CIDTFD-TPO-FAIXA  EQUAL 1
              PERFORM 3300-TIPO-FAIXA-QUANTIDADE
           ELSE
              PERFORM 3400-TIPO-FAIXA-VALOR
           END-IF.

           WRITE FD-REG-RELATORI     FROM  LINDET1-REL1.
           PERFORM 1200-TESTAR-FS-RELATORI.

           ADD 1                       TO  ACU-LINHAS
                                           ACU-GRAVA-RELATORI.

      *---------------------------------------------------------------*
       3100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA IMPRIMIR-CABECALHO
      *===============================================================*
       3200-IMPRIMIR-CABECALHO  SECTION.
      *---------------------------------------------------------------*

           ADD  1                      TO  ACU-PAGINAS
           MOVE ACU-PAGINAS            TO  CB02-REL1-PAGINA

           WRITE FD-REG-RELATORI     FROM  CABEC1-REL1.
           PERFORM 1200-TESTAR-FS-RELATORI.

           WRITE FD-REG-RELATORI     FROM  CABEC2-REL1.
           PERFORM 1200-TESTAR-FS-RELATORI.

           WRITE FD-REG-RELATORI     FROM  CABEC3-REL1.
           PERFORM 1200-TESTAR-FS-RELATORI.

           WRITE FD-REG-RELATORI     FROM  CABEC4-REL1.
           PERFORM 1200-TESTAR-FS-RELATORI.

           WRITE FD-REG-RELATORI     FROM  CABEC5-REL1.
           PERFORM 1200-TESTAR-FS-RELATORI.

           WRITE FD-REG-RELATORI     FROM  CABEC6-REL1.
           PERFORM 1200-TESTAR-FS-RELATORI.

           MOVE 6                      TO  ACU-LINHAS.

      *---------------------------------------------------------------*
       3200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA PARA FORMATAR O TIPO DE FAIXA = QUANTIDADE
      *===============================================================*
       3300-TIPO-FAIXA-QUANTIDADE  SECTION.
      *---------------------------------------------------------------*

           MOVE WBBF-CONSE-DADO-ANT        TO  WRK-CONSE-DADO-S9-15.

           MOVE WRK-CONSE-DADO-S9-05       TO  WRK-MASC-QUANTIDADE.

           MOVE WRK-QUANTIDADE             TO  LD1-REL1-VL-SIGB-ANT.

           MOVE WBBF-CONSE-DADO-ATU        TO  WRK-CONSE-DADO-S9-15.

           MOVE WRK-CONSE-DADO-S9-05       TO  WRK-MASC-QUANTIDADE.

           MOVE WRK-QUANTIDADE             TO  LD1-REL1-VL-SIGB-ATU.

           MOVE 'QUANT.'                   TO  LD1-REL1-TIPO-FAIXA.

           IF WBBF-23-PDESC-BONIF GREATER ZEROS
              MOVE WBBF-23-PDESC-BONIF TO  WRK-MASC-PERC-DESC
              MOVE WRK-PERC-DESC       TO  LD1-REL1-FRANQUIA
           ELSE
               IF  WBBF-23-QFRANQ-BONIF  GREATER ZEROS
                   MOVE WBBF-23-QFRANQ-BONIF TO  WRK-MASC-QFRANQ
                   MOVE WRK-QFRANQ-DESC      TO  LD1-REL1-FRANQUIA
               ELSE
                  MOVE WBBF-23-VCOBR-BONIF    TO  WRK-MASC-VALOR-COBR
                  MOVE WRK-VALOR-COBR         TO  LD1-REL1-FRANQUIA
               END-IF
           END-IF.

      *---------------------------------------------------------------*
       3300-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA PARA FORMATAR O TIPO DE FAIXA = VALOR
      *===============================================================*
       3400-TIPO-FAIXA-VALOR       SECTION.
      *---------------------------------------------------------------*

           MOVE WBBF-CONSE-DADO-ANT-R   TO   WRK-MASC-VALOR.

           MOVE WRK-VALOR               TO  LD1-REL1-VL-SIGB-ANT.

           MOVE WBBF-CONSE-DADO-ATU-R   TO   WRK-MASC-VALOR.

           MOVE WRK-VALOR               TO  LD1-REL1-VL-SIGB-ATU.

           MOVE 'VALOR '                TO  LD1-REL1-TIPO-FAIXA.

           IF  WBBF-23-QFRANQ-BONIF  GREATER ZEROS
               MOVE WBBF-23-QFRANQ-BONIF TO  WRK-MASC-QFRANQ
               MOVE WRK-QFRANQ-DESC      TO  LD1-REL1-FRANQUIA
           ELSE
               IF WBBF-23-PDESC-BONIF GREATER ZEROS
                  MOVE WBBF-23-PDESC-BONIF TO  WRK-MASC-PERC-DESC
                  MOVE WRK-PERC-DESC       TO  LD1-REL1-FRANQUIA
               ELSE
                  MOVE WBBF-23-VCOBR-BONIF    TO  WRK-MASC-VALOR-COBR
                  MOVE WRK-VALOR-COBR         TO  LD1-REL1-FRANQUIA
               END-IF
           END-IF.

      *---------------------------------------------------------------*
       3400-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA FINAL
      *===============================================================*
       4000-ROTINA-FINAL   SECTION.
      *---------------------------------------------------------------*

      *--> IMPRIMIR LINHA DE TOTAL

           IF  ACU-LIDOS-ARQHISTE     EQUAL ZEROS
               NEXT SENTENCE
           ELSE
               PERFORM 4100-IMPRIMIR-TOTAL
           END-IF.

      *--> MOSTRAR TOTALIZACAO

           MOVE ACU-LIDOS-ARQHISTE    TO  WRK-MASK-LIDOS-ARQHISTE
           MOVE ACU-GRAVA-RELATORI    TO  WRK-MASK-GRAVA-RELATORI

           DISPLAY '*********************** GFCT4468 *******************
      -                                                        '******'.
           DISPLAY '*
      -                                                        '     *'.
           DISPLAY '* QTDE DE REGISTROS LIDOS ARQHISTE...... : '
                                       WRK-MASK-LIDOS-ARQHISTE ' *'.
           DISPLAY '* QTDE DE REGISTROS GRAVADOS RELATORI... : '
                                       WRK-MASK-GRAVA-RELATORI ' *'.
           DISPLAY '*
      -                                                        '     *'.
           DISPLAY '*********************** GFCT4468 *******************
      -                                                        '******'.

      *---------------------------------------------------------------*
       4000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA IMPRIMIR-TOTAL
      *===============================================================*
       4100-IMPRIMIR-TOTAL SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO.

           WRITE FD-REG-RELATORI     FROM  CABEC6-REL1.
           PERFORM 1200-TESTAR-FS-RELATORI.

           ADD 1                       TO  ACU-LINHAS.

           IF  ACU-LINHAS              GREATER 59
               PERFORM 3200-IMPRIMIR-CABECALHO
           END-IF.

           MOVE ACU-GRAVA-RELATORI        TO  WRK-MASK-GRAVA-RELATORI.
           MOVE WRK-MASK-GRAVA-RELATORI   TO  LT1-REL1-TOTAIS.

           WRITE FD-REG-RELATORI     FROM  LINTOT1-REL1.
           PERFORM 1200-TESTAR-FS-RELATORI.

      *---------------------------------------------------------------*
       4100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA ERRO
      *===============================================================*
       9999-ROTINA-ERRO SECTION.
      *---------------------------------------------------------------*

           MOVE 'APL'                  TO  ERR-TIPO-ACESSO
           MOVE 'GFCT4468'             TO  ERR-PGM

           CALL 'BRAD7100'          USING  WRK-BATCH
                                           ERRO-AREA.

           GOBACK.

      *---------------------------------------------------------------*
       9999-99-FIM. EXIT.
      *---------------------------------------------------------------*
