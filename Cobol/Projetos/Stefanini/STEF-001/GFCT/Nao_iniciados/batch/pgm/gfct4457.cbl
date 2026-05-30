      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT4457.
********ROGRAM-ID. GFCT4456.
       AUTHOR.     CIBELE BARBOSA.
      *================================================================*
      * CORRECAO  EM 31/10/2016 - ALTERACAO NOME INTERNO PARA          *
      * PROMOVER NO HEXA - PROJETO PROGRAMA DE BENEFICIOS              *
      *================================================================*
      *================================================================*
      *                      S O N D A    I T                          *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT4456                                     *
      *    ANALISTA     : CIBELE BARBOSA         - SONDA IT            *
      *    DATA         : ABRIL/2014                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      VERIFICAR EM QUAL FAIXA DE BONIFICACAO O EVENTO SE ENCON- *
      *      TRA, E GRAVA ARQUIVO PARA ATUALIZAR SIGB PRV.             *
      *      BASEADO NO PGM GFCT1658.                                  *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *     DB2                                                        *
      *      TABLE                                INCLUDE/BOOK         *
      *      DB2PRD.BONIF_SERVC_TARIF               GFCTB023           *
      *      DB2PRD.BONIF_SERVC_VLR                 GFCTB024           *
      *      DB2PRD.BONIF_SERVC_QTD                 GFCTB022           *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      BRAD1050 - CONEXAO COM DB2.                               *
      *      BRAD7100 - TRATAMENTO DE ERRO                             *
      *      GFCT5204 - MODULO QUE VERIFICA SE BONIFICACAO POSSUI RES- *
      *                 TRICAO.                                        *
      *                                                                *
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

           SELECT APURA22B ASSIGN      TO UT-S-APURA22B
                      FILE STATUS      IS WRK-FS-APURA22B.

           SELECT SAIDA22C ASSIGN      TO UT-S-SAIDA22C
                      FILE STATUS      IS WRK-FS-SAIDA22C.

           SELECT APURA22C ASSIGN      TO UT-S-APURA22C
                      FILE STATUS      IS WRK-FS-APURA22C.

           SELECT ATUSIGBS ASSIGN      TO UT-S-ATUSIGBS
                      FILE STATUS      IS WRK-FS-ATUSIGBS.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT  :  ARQUIVO DE ENTRADA APURA22B - GFCTWBBE            *
      *              ORG. SEQUENCIAL   -    LRECL = 320                *
      *----------------------------------------------------------------*

       FD  APURA22B
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-APURA22B                 PIC  X(320).

      *----------------------------------------------------------------*
      *    OUTPUT :  ARQUIVO DE SAIDA SAIDA22C - I#GFCTVA              *
      *              ORG. SEQUENCIAL   -    LRECL = 250                *
      *----------------------------------------------------------------*

       FD  SAIDA22C
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-SAIDA22C                 PIC  X(250).

      *----------------------------------------------------------------*
      *    OUTPUT : ARQUIVO DE SAIDA APURA22C - GFCTWBBE               *
      *             ORG. SEQUENCIAL    -    LRECL = 320                *
      *----------------------------------------------------------------*

       FD  APURA22C
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-APURA22C                 PIC  X(320).

      *----------------------------------------------------------------*
      *    OUTPUT : ARQUIVO DE SAIDA ATUSIGBS - GFCTWBBF               *
      *             ORG. SEQUENCIAL    -    LRECL = 140                *
      *----------------------------------------------------------------*

       FD  ATUSIGBS
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ATUSIGBS                 PIC  X(140).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*  INICIO DA WORKING GFCT4456  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*         ACUMULADORES         *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05  ACU-LIDOS-APURA22B      PIC  9(07) COMP-3   VALUE ZEROS.
           05  ACU-GRAVA-SAIDA22C      PIC  9(07) COMP-3   VALUE ZEROS.
           05  ACU-GRAVA-APURA22C      PIC  9(07) COMP-3   VALUE ZEROS.
           05  ACU-GRAVA-ATUSIGBS      PIC  9(07) COMP-3   VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*          AUXILIARES          *'.
      *----------------------------------------------------------------*

       01  WRK-BATCH                   PIC  X(08)          VALUE
           'BATCH'.
       01  WRK-AUXILIARES.
           05  WRK-MODULO              PIC  X(08)          VALUE SPACES.
           05  WRK-MASCARA             PIC  Z.ZZZ.ZZ9      VALUE ZEROS.
           05  WRK-RESTRICAO-S-N       PIC  X(01)          VALUE SPACES.

           05  WRK-CONSE-DADO-S9-15    PIC S9(15)          VALUE ZEROS.
           05  FILLER                  REDEFINES WRK-CONSE-DADO-S9-15.
               10  FILLER              PIC  9(10).
               10  WRK-CONSE-DADO-S9-05
                                       PIC S9(05).

           05  WRK-CONSE-DADO-CONS-S9-15
                                       PIC S9(15)          VALUE ZEROS.
           05  FILLER                  REDEFINES
                                              WRK-CONSE-DADO-CONS-S9-15.
               10  FILLER              PIC  9(04).
               10  WRK-CONSE-DADO-S9-09
                                       PIC S9(09)V99.

           05  WRK-S9-09               PIC +9(09)          VALUE ZEROS.
           05  WRK-FILLER              REDEFINES WRK-S9-09.
               10 FILLER               PIC  X(01).
               10 WRK-9-09             PIC  9(09).

           05  WRK-S9-05               PIC +9(05)          VALUE ZEROS.
           05  WRK-FILLER              REDEFINES WRK-S9-05.
               10 FILLER               PIC  X(01).
               10 WRK-9-05             PIC  9(05).

           05  WRK-S9-03               PIC +9(03)          VALUE ZEROS.
           05  WRK-FILLER              REDEFINES WRK-S9-03.
               10 FILLER               PIC  X(01).
               10 WRK-9-03             PIC  9(03).

           05  WRK-DATA-10             PIC  X(10)         VALUE SPACES.
           05  WRK-FILLER              REDEFINES WRK-DATA-10.
               10 WRK-DD-10            PIC  9(02).
               10 FILLER               PIC  X(01).
               10 WRK-MM-10            PIC  9(02).
               10 FILLER               PIC  X(01).
               10 WRK-AAAA-10          PIC  9(04).
           05  WRK-DATA-06             PIC  9(06)         VALUE ZEROS.
           05  WRK-FILLER              REDEFINES WRK-DATA-06.
               10 WRK-AAAA-06          PIC  9(04).
               10 WRK-MM-06            PIC  9(02).

           05  WRK-SALDO-SIGB          PIC S9(15) COMP-3  VALUE ZEROS.
           05  WRK-VLR-SALDO-SIGB      REDEFINES WRK-SALDO-SIGB
                                       PIC S9(13)V99 COMP-3.


      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       AREA PARA CHAVES       *'.
      *----------------------------------------------------------------*

       01  WRK-CHV-ATU.
ST25X6*    05 WRK-CHV-CNPJ             PIC  9(09)       VALUE ZEROS.
ST25X6     05 WRK-CHV-CNPJ             PIC  X(009)       VALUE SPACES.
ST25X6*    05 WRK-CHV-FILIAL           PIC  9(05)       VALUE ZEROS.
ST25X6     05 WRK-CHV-FILIAL           PIC  X(005)       VALUE SPACES.
           05 WRK-CHV-CTRL             PIC 9(02)       VALUE ZEROS.
           05 WRK-CHV-AG               PIC 9(05)       VALUE ZEROS.
           05 WRK-CHV-CART             PIC 9(05)       VALUE ZEROS.
           05 WRK-CHV-ANOMES           PIC 9(06)       VALUE ZEROS.

       01  WRK-CHV-ANT                 PIC X(32)       VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*    AREA DE FILE-STATUS       *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-FILE-STATUS.
           05  WRK-FS-APURA22B         PIC  X(02)          VALUE SPACES.
           05  WRK-FS-SAIDA22C         PIC  X(02)          VALUE SPACES.
           05  WRK-FS-APURA22C         PIC  X(02)          VALUE SPACES.
           05  WRK-FS-ATUSIGBS         PIC  X(02)          VALUE SPACES.
           05  WRK-ABERTURA            PIC  X(13)          VALUE
               ' NA ABERTURA'.
           05  WRK-LEITURA             PIC  X(13)          VALUE
               ' NA LEITURA'.
           05  WRK-GRAVACAO            PIC  X(13)          VALUE
               ' NA GRAVACAO'.
           05  WRK-FECHAMENTO          PIC  X(13)          VALUE
               'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '* AREA PARA MENSAGENS DE ERRO  *'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGEM-ERRO.
           05  FILLER                  PIC  X(07)          VALUE SPACES.
           05  FILLER                  PIC  X(07)          VALUE
               '* ERRO '.
           05  WRK-OPERACAO            PIC  X(13)          VALUE SPACES.
           05  FILLER                  PIC  X(12)          VALUE
               ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC  X(08)          VALUE SPACES.
           05  FILLER                  PIC  X(17)          VALUE
               ' - FILE-STATUS = '.
           05  WRK-FILE-STATUS         PIC  X(02)          VALUE SPACES.
           05  FILLER                  PIC  X(02)          VALUE ' *'.
           05  FILLER                  PIC  X(07)          VALUE SPACES.

       01  WRK-MSG-ERRO-MODULO.
           05  FILLER                  PIC  X(07)          VALUE SPACES.
           05  FILLER                  PIC  X(27)          VALUE
               '* ERRO NO ACESSO AO MODULO '.
           05  WRK-MODULO-ERRO         PIC  X(08)          VALUE SPACES.
           05  FILLER                  PIC  X(24)          VALUE
               ' - GFCTZW-COD-RETORNO - '.
           05  WRK-RETURN-CODE         PIC  9(01)          VALUE ZEROS.
           05  FILLER                  PIC  X(02)          VALUE ' *'.
           05  FILLER                  PIC  X(06)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*  AREA DO ARQUIVO APURA22B/APURA22C  *'.
      *----------------------------------------------------------------*

CI0416 COPY GFCTWBBG.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*  AREA DO ARQUIVO SAIDA22C   *'.
      *----------------------------------------------------------------*

       COPY I#GFCTVA.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*  AREA DO ARQUIVO ATUSIGBS   *'.
      *----------------------------------------------------------------*

CI0416 COPY GFCTWBBH.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*       AREA DO GFCT5204     *'.
      *----------------------------------------------------------------*

       COPY I#GFCTZW.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*       AREA DA BRAD7100     *'.
      *----------------------------------------------------------------*

       COPY I#BRAD7C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '*      AREA DE TABELAS DB2     *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB023
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB024
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB022
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*    FIM DA WORKING GFCT4456   *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL                                            *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 0100-INICIALIZAR-PROGRAMA.

           PERFORM 0700-VERIFICAR-VAZIO.

           PERFORM 0900-PROCESSAR      UNTIL WRK-FS-APURA22B
                                       EQUAL '10'.

           PERFORM 1800-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA DE INICIALIZACAO DO PROGRAMA                         *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD1050'.

           MOVE SPACES                 TO WRK-AUXILIARES.

           INITIALIZE WRK-AUXILIARES
                      ACU-ACUMULADORES.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           OPEN INPUT  APURA22B
                OUTPUT SAIDA22C
                       APURA22C
                       ATUSIGBS.

           PERFORM 0200-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TESTE DE FILE-STATUS DOS ARQUIVOS               *
      *----------------------------------------------------------------*
       0200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 0300-TESTAR-FS-APURA22B.

           PERFORM 0400-TESTAR-FS-SAIDA22C.

           PERFORM 0500-TESTAR-FS-APURA22C.

           PERFORM 0600-TESTAR-FS-ATUSIGBS.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TESTE DE FILE-STATUS ARQUIVO APURA22B           *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-APURA22B         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-APURA22B         NOT EQUAL '00'
               MOVE 'APURA22B'         TO WRK-NOME-ARQ
               MOVE WRK-FS-APURA22B    TO WRK-FILE-STATUS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MENSAGEM-ERRO  TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TESTE DE FILE-STATUS ARQUIVO SAIDA22C           *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-SAIDA22C         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SAIDA22C         NOT EQUAL '00'
               MOVE 'SAIDA22C'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SAIDA22C    TO WRK-FILE-STATUS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MENSAGEM-ERRO  TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TESTE DE FILE-STATUS DO APURA22C                *
      *----------------------------------------------------------------*
       0500-TESTAR-FS-APURA22C         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-APURA22C         NOT EQUAL '00'
               MOVE 'APURA22C'         TO WRK-NOME-ARQ
               MOVE WRK-FS-APURA22C    TO WRK-FILE-STATUS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MENSAGEM-ERRO  TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TESTE DE FILE-STATUS DO ATUSIGBS                *
      *----------------------------------------------------------------*
       0600-TESTAR-FS-ATUSIGBS         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ATUSIGBS         NOT EQUAL '00'
               MOVE 'ATUSIGBS'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ATUSIGBS    TO WRK-FILE-STATUS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MENSAGEM-ERRO  TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA VERIFICAR SE O ARQUIVO DE ENTRADA ESTA VAZIO    *
      *----------------------------------------------------------------*
       0700-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 0800-LER-APURA22B.

           IF  WRK-FS-APURA22B         EQUAL '10'
               DISPLAY '************** GFCT4456 **************'
               DISPLAY '*                                    *'
               DISPLAY '*       ARQUIVO APURA22B VAZIO       *'
               DISPLAY '*                                    *'
               DISPLAY '*     PROCESSAMENTO   FINALIZADO     *'
               DISPLAY '*                                    *'
               DISPLAY '************** GFCT4456 **************'
           END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA LEITURA ARQUIVO APURA22B                        *
      *----------------------------------------------------------------*
       0800-LER-APURA22B               SECTION.
      *----------------------------------------------------------------*

           READ APURA22B               INTO GFCTWBBE-REG.

           IF  WRK-FS-APURA22B         EQUAL '10'
               GO                      TO 0800-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-APURA22B.
ST25X6*    MOVE WBBE-CPF-CNPJ          TO WRK-S9-09
ST25X6*    MOVE WRK-9-09               TO WRK-CHV-CNPJ.
ST25X6*    MOVE WBBE-FILIAL            TO WRK-S9-05
ST25X6*    MOVE WRK-9-05               TO WRK-CHV-FILIAL.
ST25X6     MOVE WBBE-CPF-CNPJ          TO WRK-CHV-CNPJ.
ST25X6     MOVE WBBE-FILIAL            TO WRK-CHV-FILIAL.
           MOVE WBBE-CONTROLE          TO WRK-CHV-CTRL.
           MOVE WBBE-CAG-DSTNO-MOVTO   TO WRK-S9-05
           MOVE WRK-9-05               TO WRK-CHV-AG.
           MOVE WBBE-23-CCART-ANLSE-GERC TO WRK-S9-05
           MOVE WRK-9-05               TO WRK-CHV-CART.

           MOVE WBBE-DOCOR-EVNTO       TO WRK-DATA-10.
           MOVE WRK-MM-10              TO WRK-MM-06.
           MOVE WRK-AAAA-10            TO WRK-AAAA-06.
           MOVE WRK-DATA-06            TO WRK-CHV-ANOMES.

           ADD 1                       TO ACU-LIDOS-APURA22B.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA DE PROCESSAMENTO PRINCIPAL DO PROGRAMA               *
      *----------------------------------------------------------------*
       0900-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

      *---------* SALVA SALDO TOTAL QUANDO MUDA CHAVE      *-----------*

           IF  WRK-CHV-ATU             NOT EQUAL WRK-CHV-ANT
               MOVE WBBE-CONSE-DADO-CONS
                                       TO WRK-SALDO-SIGB
               MOVE WRK-CHV-ATU        TO WRK-CHV-ANT
           END-IF.

           IF  WRK-SALDO-SIGB          LESS THAN ZEROS
               PERFORM 1200-GRAVAR-SAIDA22C
               GO TO   0900-10-LER
           END-IF.

           IF  WBBE-23-CIDTFD-TPO-FAIXA  OF GFCTWBBE-REG
                                       EQUAL 1
               PERFORM 1000-VERIFICAR-FAIXA-QTDE
           ELSE
               IF  WBBE-23-CIDTFD-TPO-FAIXA OF GFCTWBBE-REG
                                       EQUAL 2
                   PERFORM 1700-VERIFICAR-FAIXA-VALOR
               ELSE
                   MOVE WBBE-23-CSERVC-TARIF OF GFCTWBBE-REG
                                     TO CSERVC-TARIF   OF GFCTB023
                   MOVE WBBE-23-CTPO-BONIF OF GFCTWBBE-REG
                                     TO CTPO-BONIF     OF GFCTB023
                   MOVE WBBE-23-CCRITR-BONIF OF GFCTWBBE-REG
                                     TO CCRITR-BONIF   OF GFCTB023
                   MOVE WBBE-23-DINIC-REGRA-BONIF OF GFCTWBBE-REG
                                     TO DINIC-REGRA-BONIF OF GFCTB023
                   MOVE 1            TO CSEQ-BONIF-SERVC  OF GFCTB023
                   PERFORM 1300-BUSCAR-VLR-BONIFICAR
               END-IF
           END-IF.

      *------------* LE PROXIMO REGISTRO *-----------------------------*
       0900-10-LER.

           PERFORM 0800-LER-APURA22B.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    BUSCA CHAVE PARCIAL DO REGISTRO NA TABELA GFCTB022          *
      *----------------------------------------------------------------*
       1000-VERIFICAR-FAIXA-QTDE       SECTION.
      *----------------------------------------------------------------*

           MOVE WBBE-23-CSERVC-TARIF   TO CSERVC-TARIF    OF GFCTB022.
           MOVE WBBE-23-CTPO-BONIF     TO CTPO-BONIF      OF GFCTB022.
           MOVE WBBE-23-CCRITR-BONIF   TO CCRITR-BONIF    OF GFCTB022.
           MOVE WBBE-23-DINIC-REGRA-BONIF
                                     TO DINIC-REGRA-BONIF OF GFCTB022.
           MOVE WRK-SALDO-SIGB       TO WRK-CONSE-DADO-S9-15.
           MOVE WRK-CONSE-DADO-S9-05 TO QMIN-FAIXA-BONIF OF GFCTB022.



           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   CTPO_BONIF,
                   CCRITR_BONIF,
                   DINIC_REGRA_BONIF,
                   CSEQ_BONIF_SERVC,
                   QMIN_FAIXA_BONIF
             INTO
                   :GFCTB022.CSERVC-TARIF,
                   :GFCTB022.CTPO-BONIF,
                   :GFCTB022.CCRITR-BONIF,
                   :GFCTB022.DINIC-REGRA-BONIF,
                   :GFCTB022.CSEQ-BONIF-SERVC,
                   :GFCTB022.QMIN-FAIXA-BONIF
             FROM   DB2PRD.BONIF_SERVC_QTD
             WHERE
                   CSERVC_TARIF       = :GFCTB022.CSERVC-TARIF       AND
                   CTPO_BONIF         = :GFCTB022.CTPO-BONIF         AND
                   CCRITR_BONIF       = :GFCTB022.CCRITR-BONIF       AND
                   DINIC_REGRA_BONIF  = :GFCTB022.DINIC-REGRA-BONIF  AND
                   QMIN_FAIXA_BONIF  <= :GFCTB022.QMIN-FAIXA-BONIF   AND
                   QMAX_FAIXA_BONIF  >= :GFCTB022.QMIN-FAIXA-BONIF
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'BONIF_SERVC_QTD   '
                                       TO ERR-DBD-TAB
                MOVE 'SELECT    '      TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0010'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 1200-GRAVAR-SAIDA22C
           ELSE
               MOVE CSERVC-TARIF       OF GFCTB022
                                       TO CSERVC-TARIF     OF GFCTB023
               MOVE CTPO-BONIF         OF GFCTB022
                                       TO CTPO-BONIF       OF GFCTB023
               MOVE CCRITR-BONIF       OF GFCTB022
                                       TO CCRITR-BONIF     OF GFCTB023
               MOVE DINIC-REGRA-BONIF  OF GFCTB022
                                       TO DINIC-REGRA-BONIF OF GFCTB023
               MOVE CSEQ-BONIF-SERVC   OF GFCTB022
                                       TO CSEQ-BONIF-SERVC OF GFCTB023
               MOVE ZEROS              TO WBBF-VMIN-FAIXA-BONIF
               MOVE QMIN-FAIXA-BONIF OF GFCTB022
                                       TO WBBF-QMIN-FAIXA-BONIF

               MOVE    WRK-SALDO-SIGB     TO WBBF-CONSE-DADO-ANT

               COMPUTE WRK-SALDO-SIGB =
                       WRK-SALDO-SIGB - WBBF-QMIN-FAIXA-BONIF
               MOVE    WRK-SALDO-SIGB
                                       TO WBBF-CONSE-DADO-ATU

               PERFORM 1300-BUSCAR-VLR-BONIFICAR

           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA GRAVACAO DO ARQUIVO ATUSIGBS                    *
      *----------------------------------------------------------------*
       1100-GRAVAR-ATUSIGBS            SECTION.
      *----------------------------------------------------------------*

           MOVE WBBE-DENVIO-MOVTO-TARIF   TO WBBF-DENVIO-MOVTO-TARIF
           MOVE WBBE-CROTNA-ORIGE-MOVTO   TO WBBF-CROTNA-ORIGE-MOVTO
           MOVE WBBE-CNRO-ARQ-MOVTO       TO WBBF-CNRO-ARQ-MOVTO
           MOVE WBBE-CSEQ-MOVTO           TO WBBF-CSEQ-MOVTO
           MOVE WBBE-CSERVC-TARIF         TO WBBF-CSERVC-TARIF
           MOVE WBBE-CBCO-DSTNO-MOVTO     TO WBBF-CBCO-DSTNO-MOVTO
           MOVE WBBE-CAG-DSTNO-MOVTO      TO WBBF-CAG-DSTNO-MOVTO
           MOVE WBBE-CCTA-DSTNO-MOVTO     TO WBBF-CCTA-DSTNO-MOVTO
           MOVE WBBE-CPF-CNPJ             TO WBBF-CPF-CNPJ
           MOVE WBBE-FILIAL               TO WBBF-FILIAL
           MOVE WBBE-CONTROLE             TO WBBF-CONTROLE
           MOVE WBBE-23-CSERVC-TARIF      TO WBBF-23-CSERVC-TARIF
           MOVE WBBE-23-CTPO-BONIF        TO WBBF-23-CTPO-BONIF
           MOVE WBBE-23-CCRITR-BONIF      TO WBBF-23-CCRITR-BONIF
           MOVE WBBE-23-DINIC-REGRA-BONIF TO WBBF-23-DINIC-REGRA-BONIF
           MOVE WBBE-23-VCOBR-BONIF       TO WBBF-23-VCOBR-BONIF
           MOVE WBBE-23-PDESC-BONIF       TO WBBF-23-PDESC-BONIF
           MOVE WBBE-23-QFRANQ-BONIF      TO WBBF-23-QFRANQ-BONIF
           MOVE WBBE-23-CCART-ANLSE-GERC  TO WBBF-23-CCART-ANLSE-GERC
           MOVE WBBE-23-CGRP-CART-SIST    TO WBBF-23-CGRP-CART-SIST
           MOVE WBBE-23-CIDTFD-TPO-FAIXA  TO WBBF-23-CIDTFD-TPO-FAIXA
           MOVE WBBE-23-CSEQ-BONIF-SERVC  TO WBBF-23-CSEQ-BONIF-SERVC
           MOVE WBBE-B043-RESTRICAO-S-N   TO WBBF-B043-RESTRICAO-S-N
           MOVE WBBE-DATA-ABERTURA        TO WBBF-DATA-ABERTURA

           MOVE WBBE-DOCOR-EVNTO          TO WRK-DATA-10
                                             WBBF-DOCOR-EVNTO.

           MOVE WRK-MM-10                 TO WRK-MM-06.
           MOVE WRK-AAAA-10               TO WRK-AAAA-06.
           MOVE WRK-DATA-06               TO WBBF-ANOMES-APURACAO.

CI0416     MOVE WBBE-PROGR-BENEF          TO WBBF-PROGR-BENEF.

           WRITE FD-ATUSIGBS              FROM GFCTWBBF-REG.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-SAIDA22C.
           ADD 1                       TO ACU-GRAVA-ATUSIGBS.


      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA GRAVACAO DO ARQUIVO SAIDA22C                    *
      *----------------------------------------------------------------*
       1200-GRAVAR-SAIDA22C            SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTVA-GFCTB092.

           INITIALIZE GFCTVA-GFCTB092.

           MOVE WBBE-REG-GFCTVA        TO GFCTVA-GFCTB092.

           WRITE FD-SAIDA22C           FROM GFCTVA-GFCTB092.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-SAIDA22C.

           ADD 1                       TO ACU-GRAVA-SAIDA22C.


      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA BUSCAR VALOR BONIFICAR                          *
      *----------------------------------------------------------------*
       1300-BUSCAR-VLR-BONIFICAR       SECTION.
      *----------------------------------------------------------------*

           DISPLAY 'TARIFA     = ' CSERVC-TARIF   OF GFCTB023.
           DISPLAY 'TIPO BONIF = ' CTPO-BONIF     OF GFCTB023.
           DISPLAY 'CRITERIO   = ' CCRITR-BONIF   OF GFCTB023.
           DISPLAY 'DT INIC    = ' DINIC-REGRA-BONIF OF GFCTB023.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   CTPO_BONIF,
                   CCRITR_BONIF,
                   DINIC_REGRA_BONIF,
                   CSEQ_BONIF_SERVC,
                   VCOBR_BONIF,
                   PDESC_BONIF,
                   QFRANQ_BONIF
             INTO
                   :GFCTB023.CSERVC-TARIF,
                   :GFCTB023.CTPO-BONIF,
                   :GFCTB023.CCRITR-BONIF,
                   :GFCTB023.DINIC-REGRA-BONIF,
                   :GFCTB023.CSEQ-BONIF-SERVC,
                   :GFCTB023.VCOBR-BONIF,
                   :GFCTB023.PDESC-BONIF,
                   :GFCTB023.QFRANQ-BONIF
             FROM   DB2PRD.BONIF_SERVC_TARIF
             WHERE
                   CSERVC_TARIF       = :GFCTB023.CSERVC-TARIF       AND
                   CTPO_BONIF         = :GFCTB023.CTPO-BONIF         AND
                   CCRITR_BONIF       = :GFCTB023.CCRITR-BONIF       AND
                   DINIC_REGRA_BONIF  = :GFCTB023.DINIC-REGRA-BONIF  AND
                   CSEQ_BONIF_SERVC   = :GFCTB023.CSEQ-BONIF-SERVC
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'BONIF_SERVC_TARIF '
                                       TO ERR-DBD-TAB
                MOVE 'SELECT    '      TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0020'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           DISPLAY 'SQLCODE = '   SQLCODE.
           IF  SQLCODE                 EQUAL +100
               PERFORM 1200-GRAVAR-SAIDA22C
           ELSE
               PERFORM 1400-MOVER-DADOS
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA MOVER DADOS E GRAVAR REGISTROS EM APURA22C      *
      *----------------------------------------------------------------*
       1400-MOVER-DADOS                SECTION.
      *----------------------------------------------------------------*

           MOVE CSEQ-BONIF-SERVC       OF GFCTB023
                                       TO WBBE-23-CSEQ-BONIF-SERVC.
           MOVE VCOBR-BONIF OF GFCTB023
                                       TO WBBE-23-VCOBR-BONIF.
           MOVE PDESC-BONIF OF GFCTB023
                                       TO WBBE-23-PDESC-BONIF.
           MOVE QFRANQ-BONIF OF GFCTB023
                                       TO WBBE-23-QFRANQ-BONIF.


           PERFORM 1500-VERIFICA-RESTRICAO.

           MOVE WRK-RESTRICAO-S-N      TO WBBE-B043-RESTRICAO-S-N
                                          WBBF-B043-RESTRICAO-S-N.

           PERFORM 1100-GRAVAR-ATUSIGBS.

           PERFORM 1600-GRAVAR-APURA22C.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA VERIFICAR EXISTENCIA DE RESTRICAO PARA BONIFIC. *
      *----------------------------------------------------------------*
       1500-VERIFICA-RESTRICAO         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTZW-GFCTB043.

           INITIALIZE GFCTZW-GFCTB043.

           MOVE WBBE-23-CSERVC-TARIF   TO WRK-S9-05.
           MOVE WRK-9-05               TO GFCTZW-CSERVC-TARIF.
           MOVE WBBE-23-CTPO-BONIF     TO WRK-S9-03.
           MOVE WRK-9-03               TO GFCTZW-CTPO-BONIF.
           MOVE WBBE-23-CCRITR-BONIF   TO WRK-S9-03
           MOVE WRK-9-03               TO GFCTZW-CCRITR-BONIF.
           MOVE WBBE-23-DINIC-REGRA-BONIF
                                       TO GFCTZW-DINIC-REGRA-BONIF.
           MOVE CSEQ-BONIF-SERVC       OF GFCTB023
                                       TO WRK-S9-03.
           MOVE WRK-9-03               TO GFCTZW-CSEQ-BONIF-SERVC.
           MOVE WBBE-DOCOR-EVNTO       TO GFCTZW-DOCOR-EVNTO.

           MOVE 'GFCT4456'             TO GFCTZW-PROGRAMA.
           MOVE 'GFCT5204'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTZW-GFCTB043.


           IF  GFCTZW-COD-RETORNO      EQUAL ZEROS
               MOVE 'S'                TO WRK-RESTRICAO-S-N
           ELSE
               IF  GFCTZW-COD-RETORNO  EQUAL 1
                   MOVE 'N'            TO WRK-RESTRICAO-S-N
               ELSE
                   DISPLAY '******************** GFCT4456 **************
      -                    '******'
                   DISPLAY '*
      -                    '     *'
                   DISPLAY '*       ERRO NO ACESSO AO MODULO'
      -                    ' ' WRK-MODULO '        *'
                   DISPLAY '*
      -                    '     *'
                   DISPLAY '*            GFCTZW-COD-RETORNO = '
      -                    ' ' GFCTZW-COD-RETORNO '             *'
                   DISPLAY '*
      -                    '     *'
                   DISPLAY '******************** GFCT4456 **************
      -                    '******'
                   MOVE 'APL'          TO ERR-TIPO-ACESSO
                   MOVE 'GFCT5204'     TO WRK-MODULO-ERRO
                   MOVE GFCTZW-COD-RETORNO
                                       TO WRK-RETURN-CODE
                   MOVE WRK-MSG-ERRO-MODULO
                                       TO ERR-TEXTO
                   PERFORM 9999-ROTINA-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA GRAVAR REGISTRO EM APURA22C                     *
      *----------------------------------------------------------------*
       1600-GRAVAR-APURA22C            SECTION.
      *----------------------------------------------------------------*

           WRITE FD-APURA22C           FROM GFCTWBBE-REG.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 0500-TESTAR-FS-APURA22C.

           ADD 1                       TO ACU-GRAVA-APURA22C.


      *----------------------------------------------------------------*
       1600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    BUSCAR CHAVE PARCIAL DO REGISTRO NA TABELA GFCTB024         *
      *----------------------------------------------------------------*
       1700-VERIFICAR-FAIXA-VALOR      SECTION.
      *----------------------------------------------------------------*

           MOVE WBBE-23-CSERVC-TARIF   TO CSERVC-TARIF OF GFCTB024.
           MOVE WBBE-23-CTPO-BONIF     TO CTPO-BONIF OF GFCTB024.
           MOVE WBBE-23-CCRITR-BONIF   TO CCRITR-BONIF OF GFCTB024.
           MOVE WBBE-23-DINIC-REGRA-BONIF
                                       TO DINIC-REGRA-BONIF OF GFCTB024.
           MOVE WRK-SALDO-SIGB         TO WRK-CONSE-DADO-CONS-S9-15.
           MOVE WRK-CONSE-DADO-S9-09   TO VMIN-FAIXA-BONIF OF GFCTB024.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   CTPO_BONIF,
                   CCRITR_BONIF,
                   DINIC_REGRA_BONIF,
                   CSEQ_BONIF_SERVC,
                   VMIN_FAIXA_BONIF
             INTO
                   :GFCTB024.CSERVC-TARIF,
                   :GFCTB024.CTPO-BONIF,
                   :GFCTB024.CCRITR-BONIF,
                   :GFCTB024.DINIC-REGRA-BONIF,
                   :GFCTB024.CSEQ-BONIF-SERVC,
                   :GFCTB024.VMIN-FAIXA-BONIF
             FROM   DB2PRD.BONIF_SERVC_VLR
             WHERE
                   CSERVC_TARIF       = :GFCTB024.CSERVC-TARIF       AND
                   CTPO_BONIF         = :GFCTB024.CTPO-BONIF         AND
                   CCRITR_BONIF       = :GFCTB024.CCRITR-BONIF       AND
                   DINIC_REGRA_BONIF  = :GFCTB024.DINIC-REGRA-BONIF  AND
                   VMIN_FAIXA_BONIF  <= :GFCTB024.VMIN-FAIXA-BONIF   AND
                   VMAX_FAIXA_BONIF  >= :GFCTB024.VMIN-FAIXA-BONIF
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'BONIF_SERVC_VLR   '
                                       TO ERR-DBD-TAB
                MOVE 'SELECT    '      TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0030'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 1200-GRAVAR-SAIDA22C
           ELSE
               MOVE CSERVC-TARIF       OF GFCTB024
                                       TO CSERVC-TARIF     OF GFCTB023
               MOVE CTPO-BONIF         OF GFCTB024
                                       TO CTPO-BONIF       OF GFCTB023
               MOVE CCRITR-BONIF       OF GFCTB024
                                       TO CCRITR-BONIF     OF GFCTB023
               MOVE DINIC-REGRA-BONIF  OF GFCTB024
                                       TO DINIC-REGRA-BONIF OF GFCTB023
               MOVE CSEQ-BONIF-SERVC   OF GFCTB024
                                       TO CSEQ-BONIF-SERVC OF GFCTB023

               MOVE ZEROS              TO WBBF-QMIN-FAIXA-BONIF
               MOVE VMIN-FAIXA-BONIF OF GFCTB024
                                       TO WBBF-VMIN-FAIXA-BONIF

               MOVE    WRK-VLR-SALDO-SIGB
                                       TO WBBF-CONSE-DADO-ANT-R
               COMPUTE WRK-VLR-SALDO-SIGB =
                       WRK-VLR-SALDO-SIGB - WBBF-VMIN-FAIXA-BONIF
               MOVE    WRK-VLR-SALDO-SIGB
                                       TO WBBF-CONSE-DADO-ATU-R

               PERFORM 1300-BUSCAR-VLR-BONIFICAR
           END-IF.

      *----------------------------------------------------------------*
       1700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA DE FINALIZACAO DO PROGRAMA                           *
      *----------------------------------------------------------------*
       1800-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 1900-EMITIR-DISPLAY.

           CLOSE APURA22B
                 SAIDA22C
                 APURA22C
                 ATUSIGBS.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       1800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA EMISSAO DE ESTATISTICAS DE PROCESSAMENTO        *
      *----------------------------------------------------------------*
       1900-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '******************** GFCT4456 *******************'.
           DISPLAY '*                                               *'.

           MOVE ACU-LIDOS-APURA22B      TO WRK-MASCARA.

           DISPLAY '* REGISTROS LIDOS EM APURA22B .....:  ' WRK-MASCARA
                   ' *'.

           MOVE ACU-GRAVA-SAIDA22C      TO WRK-MASCARA.

           DISPLAY '* REGISTROS GRAVADOS NO SAIDA22C...:  ' WRK-MASCARA
                   ' *'.

           MOVE ACU-GRAVA-APURA22C      TO WRK-MASCARA.

           DISPLAY '* REGISTROS GRAVADOS NO APURA22C...:  ' WRK-MASCARA
                   ' *'.
           MOVE ACU-GRAVA-ATUSIGBS      TO WRK-MASCARA.

           DISPLAY '* REGISTROS GRAVADOS NO ATUSIGBS...:  ' WRK-MASCARA
                   ' *'.
           DISPLAY '*                                               *'.
           DISPLAY '******************** GFCT4456 *******************'.

      *----------------------------------------------------------------*
       1900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERROS                             *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT4456'             TO ERR-PGM.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

