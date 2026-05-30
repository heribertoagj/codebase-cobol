      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*

       PROGRAM-ID. GFCT4114.

      *================================================================*
      *                  SONDA PROCWORK INFORMATICA                    *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA.........: GFCT4114                                 *
      *    PROGRAMADOR......: FABRICA SOFTWARE        - SONDA PROCWORK *
      *                                                                *
      *    ANALISTA.........: ANTONIO PAGNOCCA NETO                    *
      *    LIDER PROJETO....: RODRIGO / MARCELO TEODORO                *
      *    GERENCIA / EQUIPE: GERENCIA-E / EQUIPE-1   - SONDA PROCWORK *
      *                                                                *
      *    GRUPO NEGOCIO....: DDS-GP50                                 *
      *    ANALISTA.........:                                          *
      *                                                                *
      *    DATA.............: DEZ/2010                                 *
      *                                                                *
      *    PROJETO..........: TARIFA PARCELADA                         *
      *                                                                *
      *    OBJETIVO.........: GERAR MOVIMENTO DE CARGA PARA TABELA.    *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                            INCLUDE/BOOK            *
      *      MOVIMENT                            GFCTWAOU              *
      *      CADASTRO                            GFCTWAOU              *
      *      CADASATU                            GFCTWAOU              *
      *      CARGA0N2                            GFCTWAOV              *
      *                                                                *
      *    MODULOS CHAMADOS.:                                          *
      *      BRAD7100 - TRATAR ERRO                                    *
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

           SELECT MOVIMENT ASSIGN TO UT-S-MOVIMENT
                      FILE STATUS IS WRK-FS-MOVIMENT.

           SELECT CADASTRO ASSIGN TO UT-S-CADASTRO
                      FILE STATUS IS WRK-FS-CADASTRO.

           SELECT CADASATU ASSIGN TO UT-S-CADASATU
                      FILE STATUS IS WRK-FS-CADASATU.

           SELECT CARGA0N2 ASSIGN TO UT-S-CARGA0N2
                      FILE STATUS IS WRK-FS-CARGA0N2.

      *===============================================================*
       DATA DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *   INPUT :     ORG. SEQUENCIAL   -   LRECL = 130               *
      *---------------------------------------------------------------*

       FD  MOVIMENT
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAOU'.

      *---------------------------------------------------------------*
      *   INPUT :     ORG. SEQUENCIAL   -   LRECL = 130               *
      *---------------------------------------------------------------*

       FD  CADASTRO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAOU'.

      *---------------------------------------------------------------*
      *   OUTPUT:     ORG. SEQUENCIAL   -   LRECL = 130               *
      *---------------------------------------------------------------*

       FD  CADASATU
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAOU'.

      *---------------------------------------------------------------*
      *   OUTPUT:     ORG. SEQUENCIAL   -   LRECL = 115               *
      *---------------------------------------------------------------*

       FD  CARGA0N2
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-CARGA0N2             PIC X(115).

      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
       77  FILLER  PIC X(32) VALUE '*  INICIO DA WORKING GFCT4114  *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*

      *---------------------------------------------------------------*
       77  FILLER  PIC X(32) VALUE '*  AREAS AUXILIARES            *'.
      *---------------------------------------------------------------*

012011 01  WRK-RRESUL.
012011     05  WRK-RRESUL-1            PIC 9(03) VALUE ZEROS.
012011     05  FILLER                  PIC X(01) VALUE '-'.
012011     05  WRK-RRESUL-2            PIC X(25) VALUE SPACES.

      *---------------------------------------------------------------*
       77  FILLER  PIC X(32) VALUE '*  AREAS DE FILE STATUS        *'.
      *---------------------------------------------------------------*

       77  WRK-BATCH                   PIC X(08) VALUE 'BATCH   '.

       77  WRK-FS-MOVIMENT             PIC X(02) VALUE SPACES.
       77  WRK-FS-CADASTRO             PIC X(02) VALUE SPACES.
       77  WRK-FS-CADASATU             PIC X(02) VALUE SPACES.
       77  WRK-FS-CARGA0N2             PIC X(02) VALUE SPACES.

       77  WRK-OPERACAO                PIC X(13) VALUE SPACES.
       77  WRK-ABERTURA                PIC X(13) VALUE 'NA ABERTURA'.
       77  WRK-LEITURA                 PIC X(13) VALUE 'NA LEITURA'.
       77  WRK-GRAVACAO                PIC X(13) VALUE 'NA GRAVACAO'.
       77  WRK-FECHAMENTO              PIC X(13) VALUE 'NO FECHAMENTO'.

      *---------------------------------------------------------------*
       77  FILLER  PIC X(32) VALUE '*  ACUMULADORES                *'.
      *---------------------------------------------------------------*

       77  ACU-LIDOS-MOVIMENT          PIC 9(09) COMP-3 VALUE ZEROS.
       77  ACU-LIDOS-CADASTRO          PIC 9(09) COMP-3 VALUE ZEROS.
       77  ACU-GRAVA-CADASATU          PIC 9(09) COMP-3 VALUE ZEROS.
       77  ACU-GRAVA-CARGA0N2          PIC 9(09) COMP-3 VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32) VALUE '*  MASCARAS DE EDICAO DE TOTAIS*'.
      *---------------------------------------------------------------*

       01  FILLER.
           05  WRK-MASK-LIDOS-MOVIMENT PIC ZZZ.ZZZ.ZZ9 VALUE SPACES.
           05  WRK-MASK-LIDOS-CADASTRO PIC ZZZ.ZZZ.ZZ9 VALUE SPACES.
           05  WRK-MASK-GRAVA-CADASATU PIC ZZZ.ZZZ.ZZ9 VALUE SPACES.
           05  WRK-MASK-GRAVA-CARGA0N2 PIC ZZZ.ZZZ.ZZ9 VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32) VALUE '*  CHAVES DOS ARQUIVOS         *'.
      *---------------------------------------------------------------*

       01  FILLER.
           05  WRK-CHAVE-MOVIMENT      PIC X(47) VALUE SPACES.
           05  WRK-CHAVE-CADASTRO      PIC X(47) VALUE SPACES.
           05  WRK-CHAVE-CADASTRO-ANT  PIC X(47) VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32) VALUE '*  AREA LAY-OUT ARQ CARGA0N2   *'.
      *---------------------------------------------------------------*

       COPY 'GFCTWAOV'.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32) VALUE '*  AREAS TRATAMENTO ERRO       *'.
      *---------------------------------------------------------------*

       01  WRK-TEXTO.
           05  FILLER                  PIC X(05) VALUE 'ERRO'.
           05  WRK-OPERACAO-TXT        PIC X(13) VALUE SPACES.
           05  FILLER                  PIC X(12) VALUE ' DO ARQUIVO '.
           05  WRK-ARQ-TXT             PIC X(08) VALUE SPACES.

       COPY 'I#BRAD7C'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
       01  FILLER  PIC X(32) VALUE '*  FIM  DA WORKING   GFCT4114  *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*

      *===============================================================*
       PROCEDURE DIVISION.
      *===============================================================*

      *===============================================================*
      *    ROTINA INICIAR
      *===============================================================*
       0000-INICIAR SECTION.
      *---------------------------------------------------------------*

           OPEN    INPUT   MOVIMENT
                           CADASTRO
                   OUTPUT  CADASATU
                           CARGA0N2.

           MOVE    WRK-ABERTURA        TO  WRK-OPERACAO.
           PERFORM 1000-TESTAR-FILE-STATUS.

           PERFORM 2000-PRIMEIRA-LEITURA.

           IF  ACU-LIDOS-CADASTRO      EQUAL   ZEROS
               GO  TO 0000-FINALIZAR
           END-IF.

           PERFORM 3000-PROCESSA
             UNTIL WRK-FS-MOVIMENT     EQUAL '10' AND
                   WRK-FS-CADASTRO     EQUAL '10'.

           PERFORM 4000-ROTINA-FINAL.

      *===============*
      * LABEL FINALIZAR
      *===============*
       0000-FINALIZAR.
      *---------------*

           CLOSE   MOVIMENT
                   CADASTRO
                   CADASATU
                   CARGA0N2

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

           PERFORM 1100-TESTAR-FS-MOVIMENT.

           PERFORM 1200-TESTAR-FS-CADASTRO.

           PERFORM 1300-TESTAR-FS-CADASATU.

           PERFORM 1400-TESTAR-FS-CARGA0N2.

      *---------------------------------------------------------------*
       1000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-MOVIMENT
      *===============================================================*
       1100-TESTAR-FS-MOVIMENT SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-MOVIMENT NOT EQUAL '00'
              DISPLAY '************** GFCT4114 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              MOVIMENT             *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-MOVIMENT
                                                 '         *'
              DISPLAY '*        PROGRAMA  ENCERRADO        *'
              DISPLAY '************** GFCT4114 *************'
              MOVE WRK-OPERACAO        TO  WRK-OPERACAO-TXT
              MOVE 'MOVIMENT'          TO  WRK-ARQ-TXT
              MOVE WRK-TEXTO           TO  ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-CADASTRO
      *===============================================================*
       1200-TESTAR-FS-CADASTRO SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-CADASTRO NOT EQUAL '00'
              DISPLAY '************** GFCT4114 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              CADASTRO             *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-CADASTRO
                                                 '         *'
              DISPLAY '*        PROGRAMA  ENCERRADO        *'
              DISPLAY '************** GFCT4114 *************'
              MOVE WRK-OPERACAO        TO  WRK-OPERACAO-TXT
              MOVE 'CADASTRO'          TO  WRK-ARQ-TXT
              MOVE WRK-TEXTO           TO  ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-CADASATU
      *===============================================================*
       1300-TESTAR-FS-CADASATU   SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-CADASATU   NOT EQUAL '00'
              DISPLAY '************** GFCT4114 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              CADASATU             *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-CADASATU
                                                 '         *'
              DISPLAY '*        PROGRAMA  ENCERRADO        *'
              DISPLAY '************** GFCT4114 *************'
              MOVE WRK-OPERACAO        TO  WRK-OPERACAO-TXT
              MOVE 'CADASATU'          TO  WRK-ARQ-TXT
              MOVE WRK-TEXTO           TO  ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1300-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-CARGA0N2
      *===============================================================*
       1400-TESTAR-FS-CARGA0N2   SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-CARGA0N2   NOT EQUAL '00'
              DISPLAY '************** GFCT4114 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              CARGA0N2             *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-CARGA0N2
                                                 '         *'
              DISPLAY '*        PROGRAMA  ENCERRADO        *'
              DISPLAY '************** GFCT4114 *************'
              MOVE WRK-OPERACAO        TO  WRK-OPERACAO-TXT
              MOVE 'CARGA0N2'          TO  WRK-ARQ-TXT
              MOVE WRK-TEXTO           TO  ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1400-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA PRIMEIRA-LEITURA
      *===============================================================*
       2000-PRIMEIRA-LEITURA SECTION.
      *---------------------------------------------------------------*

           PERFORM 2100-LER-MOVIMENT.

           IF  ACU-LIDOS-MOVIMENT      EQUAL   ZEROS
               DISPLAY '************** GFCT4114 *************'
               DISPLAY '*                                   *'
               DISPLAY '*    ARQUIVO MOVIMENT ESTA VAZIO    *'
               DISPLAY '*                                   *'
               DISPLAY '************** GFCT4114 *************'
           END-IF.

           PERFORM 2200-LER-CADASTRO.

           IF  ACU-LIDOS-CADASTRO      EQUAL   ZEROS
               DISPLAY '************** GFCT4114 *************'
               DISPLAY '*                                   *'
               DISPLAY '*    ARQUIVO CADASTRO ESTA VAZIO    *'
               DISPLAY '*                                   *'
               DISPLAY '*         PROGRAMA ENCERRADO        *'
               DISPLAY '*                                   *'
               DISPLAY '************** GFCT4114 *************'
           END-IF.

      *---------------------------------------------------------------*
       2000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA LER-MOVIMENT
      *===============================================================*
       2100-LER-MOVIMENT SECTION.
      *---------------------------------------------------------------*

           READ MOVIMENT.

           IF  WRK-FS-MOVIMENT         EQUAL   '10'
               MOVE  HIGH-VALUES       TO  WRK-CHAVE-MOVIMENT
               GO                      TO  2100-99-FIM
           END-IF.

           MOVE    WRK-LEITURA         TO  WRK-OPERACAO.
           PERFORM 1100-TESTAR-FS-MOVIMENT.

           ADD 1                       TO  ACU-LIDOS-MOVIMENT

           MOVE WAOU-CHAVE-GFCT        OF  MOVIMENT
                                       TO  WRK-CHAVE-MOVIMENT.

      *---------------------------------------------------------------*
       2100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA LER-CADASTRO
      *===============================================================*
       2200-LER-CADASTRO SECTION.
      *---------------------------------------------------------------*

           READ CADASTRO.

           IF  WRK-FS-CADASTRO      EQUAL   '10'
               MOVE  HIGH-VALUES       TO  WRK-CHAVE-CADASTRO
               GO                      TO  2200-99-FIM
           END-IF.

           MOVE    WRK-LEITURA         TO  WRK-OPERACAO.
           PERFORM 1200-TESTAR-FS-CADASTRO.

           ADD 1                       TO  ACU-LIDOS-CADASTRO.

           MOVE WAOU-CHAVE-GFCT        OF  CADASTRO
                                       TO  WRK-CHAVE-CADASTRO.

      *---------------------------------------------------------------*
       2200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA PROCESSA
      *===============================================================*
       3000-PROCESSA SECTION.
      *---------------------------------------------------------------*

           IF  WRK-CHAVE-MOVIMENT      EQUAL WRK-CHAVE-CADASTRO

               MOVE WRK-CHAVE-CADASTRO TO  WRK-CHAVE-CADASTRO-ANT
               PERFORM UNTIL WRK-FS-CADASTRO EQUAL '10'
                 OR WRK-CHAVE-CADASTRO NOT EQUAL WRK-CHAVE-CADASTRO-ANT
                   PERFORM 3100-GRAVAR-CARGA0N2
                   PERFORM 2200-LER-CADASTRO
               END-PERFORM

               PERFORM 2100-LER-MOVIMENT
           ELSE
               IF  WRK-CHAVE-MOVIMENT LESS WRK-CHAVE-CADASTRO
                   PERFORM 3300-EXIBIR-DISPLAY
                   PERFORM 2100-LER-MOVIMENT
               ELSE
                   PERFORM 3200-GRAVAR-CADASATU
                   PERFORM 2200-LER-CADASTRO
               END-IF
           END-IF.

      *---------------------------------------------------------------*
       3000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA GRAVAR-CARGA0N2
      *===============================================================*
       3100-GRAVAR-CARGA0N2  SECTION.
      *---------------------------------------------------------------*

           MOVE WAOU-BANCO             OF  MOVIMENT
                                       TO  WAOV-CBCO-DEB-MOVTO
           MOVE WAOU-AGE-DEB           OF  MOVIMENT
                                       TO  WAOV-CAG-DEB-MOVTO
           MOVE WAOU-CTA-DEB           OF  MOVIMENT
                                       TO  WAOV-CCTA-DEB-MOVTO
           MOVE WAOU-DTA-CONSULTA      OF  MOVIMENT
                                       TO  WAOV-DCONS-NGTVC-CLI
           MOVE WAOU-CPF-CGC-NUM       OF  MOVIMENT
                                       TO  WAOV-CCNPJ-CPF-DEB
           MOVE WAOU-CPF-CGC-FILIAL    OF  MOVIMENT
                                       TO  WAOV-CFLIAL-CNPJ-DEB
           MOVE WAOU-CPF-CTR           OF  MOVIMENT
                                       TO  WAOV-CCTRL-CNPJ-CPF-DEB
           MOVE WAOU-RES-CONSULTA      OF  MOVIMENT
                                       TO  WAOV-CINDCD-RESUL-NGTVC

012011     IF  WAOU-RES-CONSULTA       OF  MOVIMENT
012011                                 EQUAL 'E'
012011         MOVE 'NADA CONSTA'      TO  WAOV-RRESUL-CONS-NGTVC
012011     ELSE
012011         IF  WAOU-RES-CONSULTA   OF  MOVIMENT
012011                                 EQUAL 'N'
012011             MOVE 'SEM RESTRICOES'
012011                                 TO  WAOV-RRESUL-CONS-NGTVC
012011         ELSE
012011             MOVE WAOU-TP-IMPED  OF  MOVIMENT
012011                                 TO  WRK-RRESUL-1
012011             MOVE WAOU-RS-IMPED  OF  MOVIMENT
012011                                 TO  WRK-RRESUL-2
012011             MOVE WRK-RRESUL     TO  WAOV-RRESUL-CONS-NGTVC
012011         END-IF
012011     END-IF.
012011
           MOVE WAOU-CROTNA-ORIGE-MOVTO
                                       OF  CADASTRO
                                       TO  WAOV-CROTNA-ORIGE-MOVTO
           MOVE WAOU-DENVIO-MOVTO-TARIF
                                       OF  CADASTRO
                                       TO  WAOV-DENVIO-MOVTO-TARIF
           MOVE WAOU-CNRO-ARQ-MOVTO    OF  CADASTRO
                                       TO  WAOV-CNRO-ARQ-MOVTO
           MOVE WAOU-CSEQ-MOVTO        OF  CADASTRO
                                       TO  WAOV-CSEQ-MOVTO

           WRITE FD-REG-CARGA0N2     FROM  GFCTWAOV-REG

           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO.
           PERFORM 1400-TESTAR-FS-CARGA0N2.

           ADD 1                       TO  ACU-GRAVA-CARGA0N2.

      *---------------------------------------------------------------*
       3100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA GRAVAR-CADASATU
      *===============================================================*
       3200-GRAVAR-CADASATU  SECTION.
      *---------------------------------------------------------------*

           WRITE GFCTWAOU-REG OF CADASATU
            FROM GFCTWAOU-REG OF CADASTRO

           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO.
           PERFORM 1300-TESTAR-FS-CADASATU.

           ADD 1                       TO  ACU-GRAVA-CADASATU.

      *---------------------------------------------------------------*
       3200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA EXIBIR-DISPLAY
      *===============================================================*
       3300-EXIBIR-DISPLAY  SECTION.
      *---------------------------------------------------------------*

           DISPLAY '************** GFCT4114 *************'
           DISPLAY '*                                   *'
           DISPLAY '*     OCORRENCIA NAO ENCONTRADA     *'
           DISPLAY '*                                   *'
           DISPLAY '* CHAVE ARQUIVO MOVIMENT:           *'
           DISPLAY '*                                   *'
           DISPLAY '* WAOU-CCGC-CPF     : ' WAOU-CCGC-CPF
                                            OF MOVIMENT
                                                  '     *'
           DISPLAY '* WAOU-CFLIAL-CGC   : ' WAOU-CFLIAL-CGC
                                            OF MOVIMENT
                                              '         *'
           DISPLAY '* WAOU-CCTRL-CPF-CGC: ' WAOU-CCTRL-CPF-CGC
                                            OF MOVIMENT
                                           '            *'
           DISPLAY '* WAOU-DSOLTC-CNST  : ' WAOU-DSOLTC-CNST
                                            OF MOVIMENT
                                                   '    *'
           DISPLAY '* WAOU-BANCO        : ' WAOU-BANCO
                                            OF MOVIMENT
                                            '           *'
           DISPLAY '* WAOU-AGE-DEB      : ' WAOU-AGE-DEB
                                            OF MOVIMENT
                                              '         *'
           DISPLAY '* WAOU-CTA-DEB      : ' WAOU-CTA-DEB
                                            OF MOVIMENT
                                                      ' *'
           DISPLAY '*                                   *'
           DISPLAY '************** GFCT4114 *************'.

      *---------------------------------------------------------------*
       3300-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA FINAL
      *===============================================================*
       4000-ROTINA-FINAL   SECTION.
      *---------------------------------------------------------------*

           MOVE ACU-LIDOS-MOVIMENT     TO  WRK-MASK-LIDOS-MOVIMENT.
           MOVE ACU-LIDOS-CADASTRO     TO  WRK-MASK-LIDOS-CADASTRO.
           MOVE ACU-GRAVA-CADASATU     TO  WRK-MASK-GRAVA-CADASATU.
           MOVE ACU-GRAVA-CARGA0N2     TO  WRK-MASK-GRAVA-CARGA0N2.

           DISPLAY '*********************** GFCT4114 *******************
      -                                                        '****'.
           DISPLAY '*
      -                                                        '   *'.
           DISPLAY '* QTDE DE REGISTROS LIDOS    MOVIMENT... : '
                                       WRK-MASK-LIDOS-MOVIMENT ' *'.
           DISPLAY '* QTDE DE REGISTROS LIDOS    CADASTRO... : '
                                       WRK-MASK-LIDOS-CADASTRO ' *'.
           DISPLAY '* QTDE DE REGISTROS GRAVADOS CADASATU... : '
                                       WRK-MASK-GRAVA-CADASATU ' *'.
           DISPLAY '* QTDE DE REGISTROS GRAVADOS CARGA0N2... : '
                                       WRK-MASK-GRAVA-CARGA0N2 ' *'.
           DISPLAY '*
      -                                                        '   *'.
           DISPLAY '*********************** GFCT4114 *******************
      -                                                        '****'.

      *---------------------------------------------------------------*
       4000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA ERRO
      *===============================================================*
       9999-ROTINA-ERRO SECTION.
      *---------------------------------------------------------------*

           MOVE 'APL'                  TO  ERR-TIPO-ACESSO
           MOVE 'GFCT4114'             TO  ERR-PGM

           CALL 'BRAD7100'          USING  WRK-BATCH
                                           ERRO-AREA.

           GOBACK.

      *---------------------------------------------------------------*
       9999-99-FIM. EXIT.
      *---------------------------------------------------------------*
