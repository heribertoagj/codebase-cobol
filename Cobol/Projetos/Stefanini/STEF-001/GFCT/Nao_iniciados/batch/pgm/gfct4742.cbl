      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*

       PROGRAM-ID. GFCT4742.

      *================================================================*
      *                       S O N D A   I T                          *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA.........: GFCT4742                                 *
      *    PROGRAMADOR......: KATIA PEGORELLI         - SONDA IT       *
      *                                                                *
      *    ANALISTA.........: ANTONIO PAGNOCCA                         *
      *    LIDER PROJETO....: RODRIGO PRADO                            *
      *    DATA.............: JAN/2016                                 *
      *                                                                *
      *    PROJETO..........: MIGRAÇÃO HSBC                            *
      *                                                                *
      *    OBJETIVO.........: RELATORIO DE BONIFICACOES POR GRUPO DE   *
      *                       CARTEIRAS - SIGB                         *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                            INCLUDE/BOOK            *
      *        ARQRELAT                          GFCTWF40              *
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

           SELECT ARQRELAT ASSIGN TO UT-S-ARQRELAT
                      FILE STATUS IS WRK-FS-ARQRELAT.

           SELECT RELATORI ASSIGN TO UT-S-RELATORI
                      FILE STATUS IS WRK-FS-RELATORI.

      *===============================================================*
       DATA DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *   INPUT :     ORG. SEQUENCIAL   -   LRECL = 100               *
      *---------------------------------------------------------------*

       FD  ARQRELAT
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-ARQRELAT             PIC X(100).

      *---------------------------------------------------------------*
      *   OUTPUT:     ORG. SEQUENCIAL   -   LRECL = 080               *
      *---------------------------------------------------------------*

       FD  RELATORI
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-RELATORI             PIC X(080).

      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
       77  FILLER  PIC X(32)  VALUE  '*  INICIO DA WORKING GFCT4742  *'.
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

       01 WRK-VALOR.
          05 WRK-MASC-VALOR       PIC Z.ZZZ.ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*   AREAS DE FILE STATUS       *'.
      *---------------------------------------------------------------*

       77  WRK-BATCH                   PIC X(08) VALUE 'BATCH   '.

       77  WRK-FS-ARQRELAT             PIC X(02) VALUE SPACES.
       77  WRK-FS-RELATORI             PIC X(02) VALUE SPACES.

       77  WRK-OPERACAO                PIC X(13) VALUE SPACES.
       77  WRK-ABERTURA                PIC X(13) VALUE 'NA ABERTURA'.
       77  WRK-LEITURA                 PIC X(13) VALUE 'NA LEITURA'.
       77  WRK-GRAVACAO                PIC X(13) VALUE 'NA GRAVACAO'.
       77  WRK-FECHAMENTO              PIC X(13) VALUE 'NO FECHAMENTO'.

      *---------------------------------------------------------------*
       77  FILLER  PIC X(32)  VALUE  '* ACUMULADORES                 *'.
      *---------------------------------------------------------------*

       77  ACU-LIDOS-ARQRELAT          PIC 9(09) COMP-3    VALUE ZEROS.
       77  ACU-GRAVA-RELATORI          PIC 9(09) COMP-3    VALUE ZEROS.

       77  ACU-PAGINAS                 PIC 9(07) COMP-3    VALUE ZEROS.
       77  ACU-LINHAS                  PIC 9(03) COMP-3    VALUE 99.

      *---------------------------------------------------------------*
       77  FILLER  PIC X(32)  VALUE  '* MASCARAS DE EDICAO DE TOTAIS *'.
      *---------------------------------------------------------------*

       77  WRK-MASK-LIDOS-ARQRELAT     PIC Z.ZZZ.ZZZ.ZZ9   VALUE ZEROS.
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

       01  WRK-CPF-CNPJ-ANT.
ST25X6*     05  WRK-CPF-PRINC-ANT       PIC 9(009) VALUE ZEROS.
ST25X6*     05  WRK-CPF-FILIAL-ANT      PIC 9(005) VALUE ZEROS.
ST25X6*     05  WRK-CPF-CTRL-ANT        PIC 9(002) VALUE ZEROS.
ST25X6      05  WRK-CPF-PRINC-ANT       PIC X(009) VALUE SPACES.
ST25X6      05  WRK-CPF-FILIAL-ANT      PIC X(004) VALUE SPACES.
ST25X6      05  WRK-CPF-CTRL-ANT        PIC X(002) VALUE SPACES.

      * ST25X6: Representa??o alfanum?rica dos campos de CPF/CNPJ anterior
       01  FILLER  REDEFINES            WRK-CPF-CNPJ-ANT.
           05  WRK-CPF-PRINC-ANT-ST     PIC X(009).
           05  WRK-CPF-FILIAL-ANT-ST    PIC X(005).
           05  WRK-CPF-CTRL-ANT-ST      PIC X(002).

       01  WRK-CPF-CNPJ-ATU.
ST25X6*     05  WRK-CPF-PRINC-ATU       PIC 9(009) VALUE ZEROS.
ST25X6*     05  WRK-CPF-FILIAL-ATU      PIC 9(005) VALUE ZEROS.
ST25X6*     05  WRK-CPF-CTRL-ATU        PIC 9(002) VALUE ZEROS.
ST25X6      05  WRK-CPF-PRINC-ATU       PIC X(009) VALUE SPACES.
ST25X6      05  WRK-CPF-FILIAL-ATU      PIC X(004) VALUE SPACES.
ST25X6      05  WRK-CPF-CTRL-ATU        PIC X(002) VALUE SPACES.

      * ST25X6: Representa??o alfanum?rica dos campos de CPF/CNPJ atual
       01  FILLER  REDEFINES            WRK-CPF-CNPJ-ATU.
           05  WRK-CPF-PRINC-ATU-ST     PIC X(009).
           05  WRK-CPF-FILIAL-ATU-ST    PIC X(004).
           05  WRK-CPF-CTRL-ATU-ST      PIC X(002).

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*   AREAS LAYOUT ARQRELAT      *'.
      *---------------------------------------------------------------*

       COPY 'GFCTWF40'.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*  LAYOUT RELATORI             *'.
      *---------------------------------------------------------------*
       01  WRK-CABEC1-REL1.
           05  FILLER                  PIC X(001) VALUE '1'.
           05  CB01-REL1-DATA          PIC X(010) VALUE SPACES.
           05  FILLER                  PIC X(012) VALUE SPACES.
           05  FILLER                  PIC X(049) VALUE
               'B A N C O  B R A D E S C O  S / A'.
           05  CB01-REL1-HORA          PIC X(008) VALUE SPACES.

       01  WRK-CABEC2-REL1.
           05  FILLER                  PIC X(001) VALUE ' '.
           05  FILLER                  PIC X(012) VALUE 'GFCT4742'.
           05  FILLER                  PIC X(053) VALUE
               'GFCT - GESTAO, FLEXIBILIZACAO E COBRANCA DE TARIFAS'.
           05  FILLER                  PIC X(005) VALUE 'PAG.'.
           05  CB02-REL1-PAGINA        PIC Z.ZZZ.ZZ9 VALUE ZEROS.

       01  WRK-CABEC3-REL1.
           05  FILLER                  PIC X(001) VALUE ' '.
           05  FILLER                  PIC X(016) VALUE SPACES.
           05  FILLER                  PIC X(047) VALUE
               'BONIFICACOES POR GRUPO DE CARTEIRAS'.

       01  WRK-CABEC4-REL1.
           05  FILLER                  PIC X(001) VALUE ' '.
           05  FILLER                  PIC X(079) VALUE ALL '-'.

       01  WRK-CABEC5-REL1.
           05  FILLER                  PIC X(001) VALUE ' '.
           05  FILLER                  PIC X(054) VALUE
               'CPF/CNPJ             AGENC           CONTA    GRUPO'.
           05  FILLER                  PIC X(025) VALUE
               'CART.          SDO.INVEST'.

       01  WRK-CABEC6-REL1.
           05  FILLER                  PIC X(080) VALUE SPACES.

       01  WRK-LINDET1-REL1.
           05  FILLER                  PIC X(001) VALUE ' '.
           05  LD1-REL1-CPF-PRINC      PIC 999999999 VALUE ZEROS.
           05  LD1-REL1-BARRA          PIC X(001) VALUE SPACES.
           05  LD1-REL1-CPF-FILIAL     PIC 99999 VALUE ZEROS.
           05  LD1-REL1-TRACO          PIC X(001) VALUE SPACES.
           05  LD1-REL1-CPF-CTRL       PIC 99 VALUE ZEROS.
           05  FILLER                  PIC X(003) VALUE SPACES.
           05  LD1-REL1-NRO-AGE        PIC Z9999 VALUE ZEROS.
           05  FILLER                  PIC X(003) VALUE SPACES.
           05  LD1-REL1-NRO-CTA        PIC ZZZZZZZZZZZZ9 VALUE ZEROS.
           05  FILLER                  PIC X(004) VALUE SPACES.
           05  LD1-REL1-GRUPO          PIC Z9999  VALUE ZEROS.
           05  FILLER                  PIC X(002) VALUE SPACES.
           05  LD1-REL1-CARTEIRA       PIC Z9999 VALUE ZEROS.
           05  FILLER                  PIC X(001) VALUE SPACES.
           05  LD1-REL1-SD-INVEST      PIC X(020) VALUE SPACES.

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
       01  FILLER  PIC X(32)  VALUE  '*  FIM  DA WORKING   GFCT4742  *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*

      *===============================================================*
       PROCEDURE DIVISION.
      *===============================================================*

      *===============================================================*
      *    ROTINA INICIAR
      *===============================================================*
       0000-INICIAR SECTION.
      *---------------------------------------------------------------*

           OPEN    INPUT   ARQRELAT
                   OUTPUT  RELATORI

           MOVE    WRK-ABERTURA        TO  WRK-OPERACAO.
           PERFORM 1000-TESTAR-FILE-STATUS.

           PERFORM 2000-ROTINA-INICIAL.

           PERFORM 3000-PROCESSA
             UNTIL WRK-FS-ARQRELAT EQUAL '10'.

           PERFORM 4000-ROTINA-FINAL.


      *===============*
      * LABEL FINALIZAR
      *===============*
       0000-FINALIZAR.
      *---------------*

           CLOSE   ARQRELAT
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

           PERFORM 1100-TESTAR-FS-ARQRELAT.

           PERFORM 1200-TESTAR-FS-RELATORI.

      *---------------------------------------------------------------*
       1000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-ARQRELAT
      *===============================================================*
       1100-TESTAR-FS-ARQRELAT SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-ARQRELAT NOT EQUAL '00'
               DISPLAY '************** GFCT4742 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*              ARQRELAT             *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQRELAT
                                                  '         *'
               DISPLAY '*         PROGRAMA ENCERRADO        *'
               DISPLAY '************** GFCT4742 *************'
               MOVE WRK-OPERACAO       TO  WRK-OPERACAO-TXT
               MOVE 'ARQRELAT'         TO  WRK-ARQ-TXT
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
               DISPLAY '************** GFCT4742 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*              RELATORI             *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-RELATORI
                                                  '         *'
               DISPLAY '*         PROGRAMA ENCERRADO        *'
               DISPLAY '************** GFCT4742 *************'
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

           PERFORM 2200-LER-ARQRELAT.

           IF  ACU-LIDOS-ARQRELAT      EQUAL   ZEROS
               DISPLAY '************* GFCT4742 *************'
               DISPLAY '*                                  *'
               DISPLAY '*   ARQUIVO ARQRELAT ESTA VAZIO    *'
               DISPLAY '*                                  *'
               DISPLAY '************* GFCT4742 *************'
           END-IF.

      *---------------------------------------------------------------*
       2000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA LER-ARQRELAT
      *===============================================================*
       2200-LER-ARQRELAT SECTION.
      *---------------------------------------------------------------*

           READ ARQRELAT                INTO GFCTWF40-REGISTRO.

           IF  WRK-FS-ARQRELAT      EQUAL  '10'
               GO                      TO  2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO  WRK-OPERACAO.
           PERFORM 1100-TESTAR-FS-ARQRELAT.

           ADD  1                      TO  ACU-LIDOS-ARQRELAT.

      * ST25X6: Tratar campos de CPF/CNPJ, Filial e Controle
      * - Se origem (GFCTWF40) vier com SPACES, considerar como ausente
      *   e mover ZEROS para ?reas num?ricas de trabalho
      * - Caso contr?rio, converter de alfanum?rico para num?rico via
      *   vari?veis de trabalho (WRK-9-9, WRK-9-4, WRK-9-2)
           IF  GFCTWF40-PRINCIPAL  EQUAL SPACES
           AND GFCTWF40-FILIAL     EQUAL SPACES
           AND GFCTWF40-CONTROLE   EQUAL ZEROS
               MOVE SPACES         TO WRK-CPF-PRINC-ATU
                                      WRK-CPF-FILIAL-ATU
               MOVE ZEROS          TO WRK-CPF-CTRL-ATU
           ELSE
               MOVE GFCTWF40-PRINCIPAL TO WRK-CPF-PRINC-ATU
               MOVE GFCTWF40-FILIAL    TO WRK-CPF-FILIAL-ATU
               MOVE GFCTWF40-CONTROLE  TO WRK-CPF-CTRL-ATU
           END-IF.

      *---------------------------------------------------------------*
       2200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA PROCESSA
      *===============================================================*
       3000-PROCESSA SECTION.
      *---------------------------------------------------------------*

           PERFORM 3100-IMPRIMIR-RELATORIO

           PERFORM 2200-LER-ARQRELAT.

      *---------------------------------------------------------------*
       3000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA IMPRIMIR-RELATORIO
      *===============================================================*
       3100-IMPRIMIR-RELATORIO     SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-GRAVACAO                TO  WRK-OPERACAO.

           IF  ACU-LINHAS                   GREATER 59
               PERFORM 3200-IMPRIMIR-CABECALHO
           END-IF.

           IF   WRK-CPF-CNPJ-ATU NOT EQUAL SPACES AND
                WRK-CPF-CNPJ-ATU NOT EQUAL ZEROS
               IF WRK-CPF-CNPJ-ATU NOT EQUAL WRK-CPF-CNPJ-ANT
                    MOVE WRK-CPF-PRINC-ATU  TO LD1-REL1-CPF-PRINC
                    MOVE WRK-CPF-FILIAL-ATU TO LD1-REL1-CPF-FILIAL
                    MOVE WRK-CPF-CTRL-ATU   TO LD1-REL1-CPF-CTRL
                    MOVE '-'                TO LD1-REL1-TRACO
                    MOVE '/'                TO LD1-REL1-BARRA

                    MOVE WRK-CPF-CNPJ-ATU   TO WRK-CPF-CNPJ-ANT

                    IF ACU-LINHAS           GREATER 59
                        PERFORM 3200-IMPRIMIR-CABECALHO
                    END-IF

                    IF ACU-LINHAS NOT EQUAL 6
                        WRITE FD-REG-RELATORI FROM WRK-CABEC6-REL1
                        PERFORM 1200-TESTAR-FS-RELATORI
                        ADD 1               TO  ACU-LINHAS
                    END-IF
               END-IF
           ELSE
               MOVE ZEROS              TO LD1-REL1-CPF-PRINC
               MOVE ZEROS              TO LD1-REL1-CPF-FILIAL
               MOVE ZEROS              TO LD1-REL1-CPF-CTRL
               MOVE '-'                TO LD1-REL1-TRACO
               MOVE '/'                TO LD1-REL1-BARRA
           END-IF.

           MOVE GFCTWF40-AGENCIA            TO LD1-REL1-NRO-AGE.

           MOVE GFCTWF40-CONTA              TO LD1-REL1-NRO-CTA.

           MOVE GFCTWF40-CGRP-CART          TO LD1-REL1-GRUPO.

           MOVE GFCTWF40-CARTEIRA           TO LD1-REL1-CARTEIRA.

           MOVE GFCTWF40-VLR-DEC            TO  WRK-MASC-VALOR.
           MOVE WRK-VALOR                   TO  LD1-REL1-SD-INVEST

           WRITE FD-REG-RELATORI            FROM  WRK-LINDET1-REL1.
           PERFORM 1200-TESTAR-FS-RELATORI.

           ADD 1                            TO  ACU-LINHAS
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

           WRITE FD-REG-RELATORI     FROM  WRK-CABEC1-REL1.
           PERFORM 1200-TESTAR-FS-RELATORI.

           WRITE FD-REG-RELATORI     FROM  WRK-CABEC2-REL1.
           PERFORM 1200-TESTAR-FS-RELATORI.

           WRITE FD-REG-RELATORI     FROM  WRK-CABEC3-REL1.
           PERFORM 1200-TESTAR-FS-RELATORI.

           WRITE FD-REG-RELATORI     FROM  WRK-CABEC4-REL1.
           PERFORM 1200-TESTAR-FS-RELATORI.

           WRITE FD-REG-RELATORI     FROM  WRK-CABEC5-REL1.
           PERFORM 1200-TESTAR-FS-RELATORI.

           WRITE FD-REG-RELATORI     FROM  WRK-CABEC6-REL1.
           PERFORM 1200-TESTAR-FS-RELATORI.

           MOVE 6                      TO  ACU-LINHAS.

      *---------------------------------------------------------------*
       3200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA FINAL
      *===============================================================*
       4000-ROTINA-FINAL   SECTION.
      *---------------------------------------------------------------*

      *--> MOSTRAR TOTALIZACAO

           MOVE ACU-LIDOS-ARQRELAT    TO  WRK-MASK-LIDOS-ARQRELAT
           MOVE ACU-GRAVA-RELATORI    TO  WRK-MASK-GRAVA-RELATORI

           DISPLAY '*********************** GFCT4742 *******************
      -                                                        '******'.
           DISPLAY '*
      -                                                        '     *'.
           DISPLAY '* QTDE DE REGISTROS LIDOS ARQRELAT...... : '
                                       WRK-MASK-LIDOS-ARQRELAT ' *'.
           DISPLAY '* QTDE DE REGISTROS GRAVADOS RELATORI... : '
                                       WRK-MASK-GRAVA-RELATORI ' *'.
           DISPLAY '*
      -                                                        '     *'.
           DISPLAY '*********************** GFCT4742 *******************
      -                                                        '******'.
      *---------------------------------------------------------------*
       4000-99-FIM. EXIT.
      *---------------------------------------------------------------*
      *===============================================================*
      *    ROTINA ERRO
      *===============================================================*
       9999-ROTINA-ERRO SECTION.
      *---------------------------------------------------------------*

           MOVE 'APL'                  TO  ERR-TIPO-ACESSO
           MOVE 'GFCT4742'             TO  ERR-PGM

           CALL 'BRAD7100'          USING  WRK-BATCH
                                           ERRO-AREA.

           GOBACK.

      *---------------------------------------------------------------*
       9999-99-FIM. EXIT.
      *---------------------------------------------------------------*
