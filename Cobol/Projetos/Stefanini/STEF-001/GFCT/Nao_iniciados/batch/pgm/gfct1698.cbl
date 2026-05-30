      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1698.
       AUTHOR. PATRICK BORGES CASSETTARI.
      *================================================================*
      **            C P M   B R A X I S  -  S I S T E M A S            *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT1698                                     *
      *    PROGRAMADOR  : PATRICK B. CASSETTARI   - CPM BRAXIS/FPOLIS  *
      *    ANALISTA CPM : MAURICIO NAPOLEAO       - CPM BRAXIS/FPOLIS  *
      *    ANALISTA     : VINICIUS C. MADUREIRA   - GRUPO 50           *
      *    DATA         : 05/11/2007                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      BALANCE LINE ENTRE CADLEGAD E ADLEGADO, GRAVANDO SAILEGAD *
      *      (ARQUIVO CONTENDO REGISTROS ADLEGADO + DATA ADESAO LEGADO *
      *      + INDICADOR DO PERIODO DE CARENCIA + QUANTIDADE MESES CA- *
      *      RENCIA).                                                  *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *     DB2                                                        *
      *      TABLE                                 INCLUDE/BOOK        *
      *      DB2PRD.PARM_SERVC_TARIF                 GFCTB0A2          *
      *                                                                *
      *    ARQUIVOS     :                                              *
      *      DDNAME                             INCLUDE/BOOK           *
      *      CADLEGAD                             I#GFCTWY             *
      *      ADLEGADO                             I#GFCTVE             *
      *      SAILEGAD                             I#GFCTWX             *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      BRAD7100 - MODULO DE TRATAMENTO DE ERRO.                  *
      *      BRAD1050 - CONEXAO COM DB2.                               *
      *                                                                *
      *================================================================*

0208  *================================================================*
0208  *                     A L T E R A C A O                          *
0208  *----------------------------------------------------------------*
0208  *    ANALISTA....:   WAGNER                - PROCWORK - GRUPO 50 *
0208  *    DATA........:   12/02/2008                                  *
0208  *    OBJETIVO....:   ATUALIZACAO DAS INCS I#GFCTVE E I#GFCTWX    *
0208  *================================================================*
      *===============================================================*
      *                   M A N U T E N C A O                         *
      *===============================================================*
      *---------------------------------------------------------------*
      *    OBJETIVO....:   ALTERACAO DO TAMANHO DA COLUNA DA TABELA   *
      *     (JC0413)       GFCTBA2 - QDIA_CAREN_TARIF DE 9(02) PARA   *
      *                    9(03) -  O ARQUIVOS DE ENTRADA E SAIDA     *
      *                    TEM O LAYOUT DA INC I#GFCTWX / I#GFCTVE    *
      *                    QUE TEM O CAMPO QDIA-CAREN-TARIF.          *
      *    OBS.........:   (APENAS RECOMPILADO)                       *
      *---------------------------------------------------------------*
      *    PROGRAMADOR.:   J.C.R.                    - SONDA IT       *
      *    ANALISTA DDS:   HELENA                    - GP.50          *
      *    ANALISTA....:   PAGNNOCA                  - SONDA IT       *
      *    DATA........:   ABRIL/2013                                 *
      *---------------------------------------------------------------*
      *===============================================================*
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

           SELECT CADLEGAD ASSIGN      TO UT-S-CADLEGAD
                      FILE STATUS      IS WRK-FS-CADLEGAD.

           SELECT ADLEGADO ASSIGN      TO UT-S-ADLEGADO
                      FILE STATUS      IS WRK-FS-ADLEGADO.

           SELECT SAILEGAD ASSIGN      TO UT-S-SAILEGAD
                      FILE STATUS      IS WRK-FS-SAILEGAD.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT  :  ARQUIVO DE ENTRADA CADLEGAD                       *
      *              ORG. SEQUENCIAL   -   LRECL = 050                 *
      *----------------------------------------------------------------*

       FD  CADLEGAD
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-CADLEGAD             PIC X(050).

      *----------------------------------------------------------------*
      *    INPUT  :  ARQUIVO DE ENTRADA ADLEGADO                       *
      *              ORG. SEQUENCIAL   -   LRECL = 300                 *
      *----------------------------------------------------------------*

       FD  ADLEGADO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-ADLEGADO             PIC X(300).

      *----------------------------------------------------------------*
      *    OUTPUT :  ARQUIVO DE SAIDA SAILEGAD                         *
      *              ORG. SEQUENCIAL   -   LRECL = 320                 *
      *----------------------------------------------------------------*

       FD  SAILEGAD
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-SAILEGAD             PIC X(320).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*  INICIO DA WORKING GFCT1698  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*     ACUMULADORES             *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05  ACU-LIDOS-CADLEGAD      PIC  9(09) COMP-3   VALUE ZEROS.
           05  ACU-LIDOS-ADLEGADO      PIC  9(09) COMP-3   VALUE ZEROS.
           05  ACU-GRAV-SAILEGAD       PIC  9(09) COMP-3   VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  WRK-BATCH               PIC X(008)          VALUE
               'BATCH'.
           05  WRK-MASCARA             PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.
           05  WRK-CAMPO-1.
               10  FILLER              PIC X(01)           VALUE SPACES.
               10  WRK-CAMPO-1-N       PIC 9(01)           VALUE ZEROS.
           05  WRK-CAMPO-1-R           REDEFINES
               WRK-CAMPO-1             PIC +9(01).
           05  WRK-CAMPO-2.
               10  FILLER              PIC X(01)           VALUE SPACES.
               10  WRK-CAMPO-2-N       PIC 9(02)           VALUE ZEROS.
           05  WRK-CAMPO-2-R           REDEFINES
               WRK-CAMPO-2             PIC +9(02).
           05  WRK-CHAVE-CADLEGAD-ANT  PIC X(22)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*     CHAVES PARA OS ARQUIVOS  *'.
      *----------------------------------------------------------------*

       01  WRK-CHAVE-CADLEGAD.
           05  WRK-CH1-CTRO-CUSTO      PIC  X(04)          VALUE SPACES.
           05  WRK-CH1-AGENCIA         PIC S9(05)          VALUE ZEROS.
           05  WRK-CH1-CONTA           PIC S9(13)          VALUE ZEROS.

       01  WRK-CHAVE-ADLEGADO.
           05  WRK-CH2-CTRO-CUSTO      PIC  X(04)          VALUE SPACES.
           05  WRK-CH2-AGENCIA         PIC S9(05)          VALUE ZEROS.
           05  WRK-CH2-CONTA           PIC S9(13)          VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*     AREA PARA FILE-STATUS    *'.
      *----------------------------------------------------------------*

       01  WRK-FS-CADLEGAD             PIC  X(02)          VALUE SPACES.
       01  WRK-FS-ADLEGADO             PIC  X(02)          VALUE SPACES.
       01  WRK-FS-SAILEGAD             PIC  X(02)          VALUE SPACES.
       01  WRK-ABERTURA                PIC  X(13)          VALUE
           'NA ABERTURA'.
       01  WRK-LEITURA                 PIC  X(13)          VALUE
           'NA LEITURA'.
       01  WRK-GRAVACAO                PIC  X(13)          VALUE
           'NA GRAVACAO'.
       01  WRK-FECHAMENTO              PIC  X(13)          VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*     MENSAGENS DE ERRO        *'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-FS.
           05  FILLER                  PIC X(07)           VALUE SPACES.
           05  FILLER                  PIC X(07)           VALUE
               '* ERRO '.
           05  WRK-OPERACAO            PIC X(13)           VALUE SPACES.
           05  FILLER                  PIC X(12)           VALUE
               ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC X(08)           VALUE SPACES.
           05  FILLER                  PIC X(17)           VALUE
               ' - FILE-STATUS = '.
           05  WRK-FS                  PIC X(02)           VALUE SPACES.
           05  FILLER                  PIC X(02)           VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)           VALUE
           '* AREA PARA DO ARQUIVO CADLEGAD  *'.
      *----------------------------------------------------------------*

       COPY I#GFCTWY.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* AREA PARA O ARQUIVO ADLEGADO *'.
      *----------------------------------------------------------------*

JC0413 COPY I#GFCTVE.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* AREA PARA O ARQUIVO SAILEGAD *'.
      *----------------------------------------------------------------*

JC0413 COPY I#GFCTWX.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*     AREA PARA BRAD7100       *'.
      *----------------------------------------------------------------*

       COPY I#BRAD7C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA DE TABELAS DB2     *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A2
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*  FIM DA WORKING GFCT1698  *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA                                *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD1050'.

           PERFORM 0100-INICIALIZAR.

           PERFORM 0600-VERIFICAR-VAZIO.

           PERFORM 0900-PROCESSAR-DADOS UNTIL WRK-CHAVE-ADLEGADO
                                       EQUAL HIGH-VALUES.

           PERFORM 1200-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA INICIAL DO PROGRAMA                                  *
      *----------------------------------------------------------------*
       0100-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  CADLEGAD
                       ADLEGADO
                OUTPUT SAILEGAD.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTE DE FILE STATUS DOS ARQUIVOS                           *
      *----------------------------------------------------------------*
       0200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 0300-TESTAR-FS-CADLEGAD.

           PERFORM 0400-TESTAR-FS-ADLEGADO.

           PERFORM 0500-TESTAR-FS-SAILEGAD.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTE DE FILE STATUS DO ARQUIVO CADLEGAD                    *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-CADLEGAD         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CADLEGAD         NOT EQUAL '00'
               MOVE 'CADLEGAD'         TO WRK-NOME-ARQ
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-CADLEGAD    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTE DE FILE STATUS DO ARQUIVO ADLEGADO                    *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-ADLEGADO         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ADLEGADO         NOT EQUAL '00'
               MOVE 'ADLEGADO'         TO WRK-NOME-ARQ
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-ADLEGADO    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTE DE FILE STATUS DO ARQUIVO SAILEGAD                    *
      *----------------------------------------------------------------*
       0500-TESTAR-FS-SAILEGAD         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SAILEGAD         NOT EQUAL '00'
               MOVE 'SAILEGAD'         TO WRK-NOME-ARQ
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-SAILEGAD    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA VERIFICAR ARQUIVO VAZIO                         *
      *----------------------------------------------------------------*
       0600-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 0700-LER-ADLEGADO.

           IF  WRK-CHAVE-ADLEGADO      EQUAL HIGH-VALUES
               DISPLAY '********** GFCT1698 ***********'
               DISPLAY '*                             *'
               DISPLAY '*   ARQUIVO ADLEGADO VAZIO    *'
               DISPLAY '*   PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                             *'
               DISPLAY '********** GFCT1698 ***********'
           END-IF.

           PERFORM 0800-LER-CADLEGAD.

           IF  WRK-CHAVE-CADLEGAD      EQUAL HIGH-VALUES
               DISPLAY '********** GFCT1698 ***********'
               DISPLAY '*                             *'
               DISPLAY '*   ARQUIVO CADLEGAD VAZIO    *'
               DISPLAY '*   PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                             *'
               DISPLAY '********** GFCT1698 ***********'
           END-IF.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO ADLEGADO                                 *
      *----------------------------------------------------------------*
       0700-LER-ADLEGADO               SECTION.
      *----------------------------------------------------------------*

           READ ADLEGADO               INTO REG-IGFCTVE.

           IF  WRK-FS-ADLEGADO         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHAVE-ADLEGADO
               GO                      TO 0700-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-ADLEGADO.

           ADD 1                       TO ACU-LIDOS-ADLEGADO.

           MOVE VE-CROTNA-ORIGE-MOVTO  TO WRK-CH2-CTRO-CUSTO.
           MOVE VE-CAG-DSTNO-MOVTO     TO WRK-CH2-AGENCIA.
           MOVE VE-CCTA-DSTNO-MOVTO    TO WRK-CH2-CONTA.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO CADLEGAD                                 *
      *----------------------------------------------------------------*
       0800-LER-CADLEGAD               SECTION.
      *----------------------------------------------------------------*

           READ CADLEGAD               INTO REG-GFCTWY.

           IF  WRK-FS-CADLEGAD         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHAVE-CADLEGAD
               GO                      TO 0800-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-CADLEGAD.

           ADD 1                       TO ACU-LIDOS-CADLEGAD.

           MOVE WY-CENTRO-CUSTO        TO WRK-CH1-CTRO-CUSTO.
           MOVE WY-AGENCIA             TO WRK-CH1-AGENCIA.
           MOVE WY-CONTA               TO WRK-CH1-CONTA.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO                                               *
      *----------------------------------------------------------------*
       0900-PROCESSAR-DADOS            SECTION.
      *----------------------------------------------------------------*

0208*******MOVE REG-IGFCTVE(1:163)     TO REG-IGFCTWX.
           MOVE REG-IGFCTVE            TO REG-IGFCTWX.

           IF  WRK-CHAVE-CADLEGAD      EQUAL WRK-CHAVE-ADLEGADO
               PERFORM 1000-BUSCAR-GFCTB0A2
               MOVE WY-DATA-ADESAO TO WX-DATA-LGDO
               PERFORM 1100-GRAVAR-SAILEGAD
               PERFORM 0700-LER-ADLEGADO
           ELSE
               IF  WRK-CHAVE-CADLEGAD  GREATER WRK-CHAVE-ADLEGADO
                   MOVE '01.01.0001'   TO WX-DATA-LGDO
                   INITIALIZE WX-IND-PER-CAR
                              WX-QTDE-MES-CAR
                   PERFORM 1100-GRAVAR-SAILEGAD
                   PERFORM 0700-LER-ADLEGADO
               ELSE
                   PERFORM 0800-LER-CADLEGAD
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA OBTER DADOS DA TABELA GFCTB0A2                  *
      *----------------------------------------------------------------*
       1000-BUSCAR-GFCTB0A2            SECTION.
      *----------------------------------------------------------------*

           MOVE VE-CSERVC-TARIF        TO CSERVC-TARIF OF GFCTB0A2.
           MOVE VE-DOCOR-EVNTO         TO DINIC-VGCIA-TARIF OF GFCTB0A2
                                          DFIM-VGCIA-TARIF OF GFCTB0A2.

           EXEC SQL
             SELECT CSERVC_TARIF,
                    CINDCD_PER_CAREN,
                    QMES_CAREN_TARIF
             INTO  :GFCTB0A2.CSERVC-TARIF,
                   :GFCTB0A2.CINDCD-PER-CAREN,
                   :GFCTB0A2.QMES-CAREN-TARIF
             FROM   DB2PRD.PARM_SERVC_TARIF
             WHERE CSERVC_TARIF       = :GFCTB0A2.CSERVC-TARIF       AND
                   DINIC_VGCIA_TARIF <= :GFCTB0A2.DINIC-VGCIA-TARIF  AND
                   DFIM_VGCIA_TARIF  >= :GFCTB0A2.DFIM-VGCIA-TARIF
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'PARM_SERVC_TARIF  '
                                       TO ERR-DBD-TAB
                MOVE 'SELECT    '      TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0010'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               INITIALIZE WX-IND-PER-CAR
                          WX-QTDE-MES-CAR
           ELSE
               MOVE CINDCD-PER-CAREN OF GFCTB0A2
                                       TO WRK-CAMPO-1-R
               MOVE WRK-CAMPO-1-N      TO WX-IND-PER-CAR
               MOVE QMES-CAREN-TARIF OF GFCTB0A2
                                       TO WRK-CAMPO-2-R
               MOVE WRK-CAMPO-2-N      TO WX-QTDE-MES-CAR
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVACAO DO ARQUIVO SAILEGAD                                *
      *----------------------------------------------------------------*
       1100-GRAVAR-SAILEGAD            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE FD-REG-SAILEGAD       FROM REG-IGFCTWX.

           PERFORM 0500-TESTAR-FS-SAILEGAD.

           ADD 1                       TO ACU-GRAV-SAILEGAD.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCEDIMENTOS FINAIS                                        *
      *----------------------------------------------------------------*
       1200-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 1300-EMITIR-TOTAIS.

           CLOSE CADLEGAD
                 ADLEGADO
                 SAILEGAD.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    EMITIR TOTAIS DE LIDOS E GRAVADOS                           *
      *----------------------------------------------------------------*
       1300-EMITIR-TOTAIS              SECTION.
      *----------------------------------------------------------------*

           DISPLAY '********************* GFCT1698 *********************
      -            '*'.
           DISPLAY '*
      -            '*'.

           MOVE ACU-LIDOS-ADLEGADO     TO WRK-MASCARA.

           DISPLAY '*  REGISTROS LIDOS EM ADLEGADO .....: '
      -                                           ' ' WRK-MASCARA '  *'.

           MOVE ACU-LIDOS-CADLEGAD     TO WRK-MASCARA.

           DISPLAY '*  REGISTROS LIDOS EM CADLEGAD .....: '
      -                                           ' ' WRK-MASCARA '  *'.

           MOVE ACU-GRAV-SAILEGAD      TO WRK-MASCARA.

           DISPLAY '*  REGISTROS GRAVADOS NO SAILEGAD...: '
      -                                           ' ' WRK-MASCARA '  *'.
           DISPLAY '*
      -            '*'.
           DISPLAY '********************* GFCT1698 *********************
      -            '*'.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    RETORNO DE ERRO                                             *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1698'             TO ERR-PGM.

           IF  ERR-TIPO-ACESSO         EQUAL 'DB2'
               CALL 'BRAD7100'         USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA
           ELSE
               CALL 'BRAD7100'         USING WRK-BATCH
                                             ERRO-AREA
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
