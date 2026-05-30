      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2135.
       AUTHOR. FELIPE DA SILVA.
      *================================================================*
      *                   C P M   S I S T E M A S                      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA.....: GFCT2135                                     *
      *    PROGRAMADOR..: FELIPE DA SILVA           CPM / PATO BRANCO  *
      *    ANALISTA CPM.: ADRIANO LOPES SANT'ANNA   CPM / PATO BRANCO  *
      *    ANALISTA.....: RICARDO PEREIRA           PROCWORK / GP.50   *
      *    DATA.........: 29/05/2006                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO.....: IDENTIFICA PAB DIFERENTE DE ZEROS ATRAVES    *
      *      DE PARM.                                                  *
      *----------------------------------------------------------------*
      *    ARQUIVOS.....:                                              *
      *       DDNAME                                INCLUDE/BOOK       *
      *         CLIEADES                               I#GFCTRU        *
      *         CLIEPAB                                I#GFCTRU        *
      *         CLIEPBC                                I#GFCTRU        *
      *         CLIEPEX                                I#GFCTRU        *
      *----------------------------------------------------------------*
      *    BANCO DE DADOS:                                             *
      *      DB2                                                       *
      *        TABLE                                INCLUDE/BOOK       *
      *        DB2PRD.POSTO_BCRIO_SERVC                MESUB062        *
      *        DB2PRD.TCAD_CORSP_BCRIO                 YMKCB001        *
      *----------------------------------------------------------------*
      *    INC'S                                                       *
      *    I#GFCTRU - ESTRUTURA DOS ARQUIVOS DE ENTRADA E SAIDA        *
      *    POOL1050 - MODULO PARA CONEXAO COM DB2                      *
      *    POL7100C - AREA DE FORMATACAO PARA POOL7100                 *
      *----------------------------------------------------------------*
      *    MODULOS                                                     *
      *    POOL7100 - MODULO PARA PROCESSAMENTO DE ERRO                *
      *================================================================*
      *                     A L T E R A C A O                          *
      *----------------------------------------------------------------*
      *    PROGRAMADOR.: GILMAR JOSE KRETZER      - CPM BRAXIS/FPOLIS  *
      *    ANALISTA CPM: ALEXANDRE PEREIRA        - CPM BRAXIS/FPOLIS  *
      *    ANALISTA....: MILLER CIUSJMAK          - GP. 45             *
      *    DATA........: 13/11/2007                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....: INCLUI O BANCO NA CONDICAO DE BUSCA DE DADOS  *
      *                  NA TABELA MESUB062.                           *
      *================================================================*
      *                    A L T E R A C A O                           *
      *----------------------------------------------------------------*
      *                                                                *
      *    ANALISTA     : MARCUS VINICIUS          - PROCWORK          *
      *    DATA         : 20/08/2008                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      INCLUSAO DO TRATAMENTO PARA BRADESCO EXPRESSO, ACESSANDO  *
      *      A BASE YMKCB001 DE CORRESPONDENTE BANC RIO, SEPARANDO-O   *
      *      DO TRATAMENTO DE BANCO POSTAL.                            *
      *                                                                *
      *================================================================*
      *                                                                *
      *    ANALISTA....:  CIBELE          - SONDA IT                   *
      *    DATA........:  06 / 2014                                    *
      *    OBJETIVO....:  SUBSTITUIR O ACESSO A BASE YMKCB001          *
      *                   PELO MODULO KCIM0184.                        *
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

           SELECT CLIEADES ASSIGN      TO UT-S-CLIEADES
                      FILE STATUS      IS WRK-FS-CLIEADES.

           SELECT CLIEPAB ASSIGN       TO UT-S-CLIEPAB
                      FILE STATUS      IS WRK-FS-CLIEPAB.

           SELECT CLIEPBC ASSIGN       TO UT-S-CLIEPBC
                      FILE STATUS      IS WRK-FS-CLIEPBC.

           SELECT CLIEPEX ASSIGN       TO UT-S-CLIEPEX
                      FILE STATUS      IS WRK-FS-CLIEPEX.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:     ARQUIVO DE ENTRADA - CLIEADES                    *
      *               ORG. SEQUENCIAL    - LRECL = 40                  *
      *----------------------------------------------------------------*

       FD  CLIEADES
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTRU'.

      *----------------------------------------------------------------*
      *   OUTPUT:     ARQUIVO DE SAIDA    - CLIEPAB                    *
      *               ORG. SEQUENCIAL     - LRECL = 040                *
      *----------------------------------------------------------------*

       FD  CLIEPAB
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTRU'.

      *----------------------------------------------------------------*
      *   OUTPUT:     ARQUIVO DE SAIDA    - CLIEPBC                    *
      *               ORG. SEQUENCIAL     - LRECL = 040                *
      *----------------------------------------------------------------*

       FD  CLIEPBC
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTRU'.

      *----------------------------------------------------------------*
      *   OUTPUT:     ARQUIVO DE SAIDA    - CLIEPEX                    *
      *               ORG. SEQUENCIAL     - LRECL = 040                *
      *----------------------------------------------------------------*

       FD  CLIEPEX
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTRU'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)          VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)          VALUE
           '* AREA PARA ACUMULADORES *'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS                   PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS                PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADO1                PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADO2                PIC  9(009) COMP-3  VALUE ZEROS.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)          VALUE
           '* AREAS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-MASK                    PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.

       77  WRK-FIM                     PIC  X(001)         VALUE SPACES.

       77  WRK-GRAVAR                  PIC  X(001)         VALUE SPACES.

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

       77  WRK-MSG                     PIC  X(028)         VALUE
           '** ARQUIVO CLIEADES VAZIO **'.

       01  WRK-CPAB-9-05               PIC  9(005)         VALUE ZEROS.
       01  WRK-FILLER                  REDEFINES WRK-CPAB-9-05.
           05 FILLER                   PIC  9(02).
           05 WRK-CPAB-9-03            PIC  9(03).

       01  WRK-CHAVE-ATU.
           05 WRK-AGENCIA-ATU          PIC  9(005)         VALUE ZEROS.
           05 WRK-POSTO-ATU            PIC  9(005)         VALUE ZEROS.

       01  WRK-CHAVE-ANT.
           05 WRK-AGENCIA-ANT          PIC  9(005)         VALUE ZEROS.
           05 WRK-POSTO-ANT            PIC  9(005)         VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TESTE DE FILE-STATUS *'.
      *----------------------------------------------------------------*

       01  WRK-FS-CLIEADES             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-CLIEPAB              PIC  X(002)         VALUE SPACES.
       01  WRK-FS-CLIEPBC              PIC  X(002)         VALUE SPACES.
       01  WRK-FS-CLIEPEX              PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA '.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* MENSAGENS DE ERRO *'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-ARQUIVO.
           05  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           05  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           05  WRK-ARQUIVO             PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(018)         VALUE
               ' - FILE STATUS =  '.
           05  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(003)         VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA DE COMUNICACAO COM KCIM0184           *'.
      *----------------------------------------------------------------*

       COPY KCIMW184.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA POOL7100 *'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE MESUB062
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LKG-PARM.
           03 LKG-TAMANHO              PIC  S9(004) COMP.
           03 LKG-IDENT                PIC   9(001).

      *================================================================*
       PROCEDURE                       DIVISION        USING LKG-PARM.
      *================================================================*

      ******************************************************************
      *  CONTROLE DO PROCESSAMENTO                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           IF  LKG-IDENT               NOT NUMERIC OR
              (LKG-IDENT               NOT EQUAL 0 AND 1 AND 2)
               DISPLAY '******** GFCT2135 ********'
               DISPLAY '*                        *'
               DISPLAY '*   PARAMETRO INVALIDO   *'
               DISPLAY '*   LKG-IDENT: ' LKG-IDENT
                                       '         *'
               DISPLAY '*                        *'
               DISPLAY '******** GFCT2135 ********'
               PERFORM 4000-FINALIZAR
           END-IF.

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ABERTURA DOS ARQUIVOS                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           OPEN INPUT   CLIEADES
                OUTPUT  CLIEPAB
                        CLIEPBC
                        CLIEPEX.

           MOVE WRK-ABERTURA           TO   WRK-OPERACAO.
           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  SECTION PARA TESTE DE FILE STATUS                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-CLIEADES.

           PERFORM 1120-TESTAR-FS-CLIEPAB.

           PERFORM 1130-TESTAR-FS-CLIEPBC.

           PERFORM 1140-TESTAR-FS-CLIEPEX.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TESTE DE FILE STATUS DO ARQUIVO CLIEADES                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-CLIEADES         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-CLIEADES          NOT EQUAL '00'
              MOVE 'CLIEADES'                TO WRK-ARQUIVO
              MOVE WRK-FS-CLIEADES           TO WRK-FILE-STATUS
              MOVE 'APL'                     TO ERR-TIPO-ACESSO
              MOVE WRK-ERRO-ARQUIVO          TO ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TESTE DE FILE STATUS DO ARQUIVO CLIEPAB                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-CLIEPAB          SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-CLIEPAB  NOT EQUAL '00'
              MOVE 'CLIEPAB '                TO WRK-ARQUIVO
              MOVE WRK-FS-CLIEPAB            TO WRK-FILE-STATUS
              MOVE 'APL'                     TO ERR-TIPO-ACESSO
              MOVE WRK-ERRO-ARQUIVO          TO ERR-TEXTO
              PERFORM  9999-ROTINA-ERRO
            END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TESTE DE FILE STATUS DO ARQUIVO CLIEPBC                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1130-TESTAR-FS-CLIEPBC          SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-CLIEPBC  NOT EQUAL '00'
              MOVE 'CLIEPBC '                TO WRK-ARQUIVO
              MOVE WRK-FS-CLIEPBC            TO WRK-FILE-STATUS
              MOVE 'APL'                     TO ERR-TIPO-ACESSO
              MOVE WRK-ERRO-ARQUIVO          TO ERR-TEXTO
              PERFORM  9999-ROTINA-ERRO
            END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TESTE DE FILE STATUS DO ARQUIVO CLIEPEX                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1140-TESTAR-FS-CLIEPEX          SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-CLIEPEX  NOT EQUAL '00'
              MOVE 'CLIEPEX '                TO WRK-ARQUIVO
              MOVE WRK-FS-CLIEPEX            TO WRK-FILE-STATUS
              MOVE 'APL'                     TO ERR-TIPO-ACESSO
              MOVE WRK-ERRO-ARQUIVO          TO ERR-TEXTO
              PERFORM  9999-ROTINA-ERRO
            END-IF.

      *----------------------------------------------------------------*
       1140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TESTE DE ARQUIVO VAZIO                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-CLIEADES.

           IF WRK-FS-CLIEADES          EQUAL '10'
              DISPLAY '********** GFCT2135 ***********'
              DISPLAY '*     ARQUIVO CLIEADES VAZIO  *'
              DISPLAY '*      PROGRAMA CANCELADO     *'
              DISPLAY '********** GFCT2135 ***********'
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE WRK-MSG             TO ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  LEITURA DO ARQUIVO CLIEADES                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-CLIEADES               SECTION.
      *----------------------------------------------------------------*

           READ CLIEADES.

           IF WRK-FS-CLIEADES          EQUAL '10'
              GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO   WRK-OPERACAO.
           PERFORM 1110-TESTAR-FS-CLIEADES.

           ADD 1                       TO   ACU-LIDOS.

           MOVE GFCTRU-AGENCIA OF CLIEADES
                                       TO   WRK-AGENCIA-ATU.
           MOVE GFCTRU-POSTO-SERV OF CLIEADES
                                       TO   WRK-POSTO-ATU.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  PROCESSAMENTO PRINCIPAL                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF LKG-IDENT                EQUAL 0
               PERFORM 3100-TRATAR-PAB UNTIL
                   WRK-FS-CLIEADES     EQUAL '10'
           END-IF.

           IF LKG-IDENT                EQUAL 1
               PERFORM 3200-TRATAR-BCO-POSTAL UNTIL
                   WRK-FS-CLIEADES     EQUAL '10'
           END-IF.

           IF LKG-IDENT                EQUAL 2
               PERFORM 3300-TRATAR-BRAD-EXPRE UNTIL
                   WRK-FS-CLIEADES     EQUAL '10'
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TRATAR PAB                                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-TRATAR-PAB                 SECTION.
      *----------------------------------------------------------------*

           IF WRK-CHAVE-ATU            NOT EQUAL WRK-CHAVE-ANT
               PERFORM 3110-OBTER-DADOS-MESUB062
               MOVE WRK-CHAVE-ATU      TO  WRK-CHAVE-ANT
           END-IF.

           IF WRK-GRAVAR               EQUAL 'S'

               MOVE GFCTRU-ENTRADA OF CLIEADES
                                       TO GFCTRU-ENTRADA OF CLIEPAB
               WRITE GFCTRU-ENTRADA    OF CLIEPAB
               MOVE WRK-GRAVACAO       TO WRK-OPERACAO
               PERFORM 1120-TESTAR-FS-CLIEPAB

               ADD 1                   TO ACU-GRAVADOS
           END-IF.

           PERFORM 2100-LER-CLIEADES.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *   ACESSO A TABELA MESUB062                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3110-OBTER-DADOS-MESUB062       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AGENCIA-ATU        TO CDEPDC OF MESUB062.
           MOVE WRK-POSTO-ATU          TO CPOSTO-SERVC OF MESUB062.
           MOVE 9                      TO CTPO-POSTO-SERVC OF MESUB062.
           MOVE 237                    TO CEMPR-INC OF MESUB062.

           EXEC SQL
             SELECT
                   CTPO_POSTO_SERVC
             INTO
                   :MESUB062.CTPO-POSTO-SERVC
             FROM   DB2PRD.POSTO_BCRIO_SERVC
             WHERE
                   CEMPR_INC          = :MESUB062.CEMPR-INC        AND
                   CDEPDC             = :MESUB062.CDEPDC           AND
                   CPOSTO_SERVC       = :MESUB062.CPOSTO-SERVC     AND
                   CTPO_POSTO_SERVC  <> :MESUB062.CTPO-POSTO-SERVC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
              MOVE 'DB2'               TO ERR-TIPO-ACESSO
              MOVE 'POSTO_BCRIO_SERVC' TO ERR-DBD-TAB
              MOVE 'SELECT  '          TO ERR-FUN-COMANDO
              MOVE  SQLCODE            TO ERR-SQL-CODE
              MOVE  '0010'             TO ERR-LOCAL
              MOVE  SPACES             TO ERR-SEGM
              PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF SQLCODE                  EQUAL ZEROS
               MOVE 'S'                TO WRK-GRAVAR
           ELSE
               MOVE 'N'                TO WRK-GRAVAR
           END-IF.

      *----------------------------------------------------------------*
       3110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TRATAR BCO POSTAL                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-TRATAR-BCO-POSTAL          SECTION.
      *----------------------------------------------------------------*

           IF WRK-CHAVE-ATU            NOT EQUAL WRK-CHAVE-ANT
               PERFORM 3210-OBTER-DADOS-MESUB062
               MOVE WRK-CHAVE-ATU      TO WRK-CHAVE-ANT
           END-IF.

           IF WRK-GRAVAR               EQUAL 'S'
               MOVE GFCTRU-ENTRADA OF CLIEADES
                                       TO GFCTRU-ENTRADA OF CLIEPBC
               WRITE GFCTRU-ENTRADA    OF CLIEPBC
               MOVE WRK-GRAVACAO       TO WRK-OPERACAO
               PERFORM 1130-TESTAR-FS-CLIEPBC

               ADD 1                   TO ACU-GRAVADO1
           END-IF.

           PERFORM 2100-LER-CLIEADES.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ACESSO A TABELA MESUB062                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       3210-OBTER-DADOS-MESUB062       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AGENCIA-ATU        TO CDEPDC OF MESUB062.
           MOVE WRK-POSTO-ATU          TO CPOSTO-SERVC OF MESUB062.
           MOVE 9                      TO CTPO-POSTO-SERVC OF MESUB062.
           MOVE 237                    TO CEMPR-INC OF MESUB062.

           EXEC SQL
             SELECT
                   CTPO_POSTO_SERVC
             INTO
                   :MESUB062.CTPO-POSTO-SERVC
             FROM   DB2PRD.POSTO_BCRIO_SERVC
             WHERE
                   CEMPR_INC          = :MESUB062.CEMPR-INC        AND
                   CDEPDC             = :MESUB062.CDEPDC           AND
                   CPOSTO_SERVC       = :MESUB062.CPOSTO-SERVC     AND
                   CTPO_POSTO_SERVC   = :MESUB062.CTPO-POSTO-SERVC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
              MOVE 'DB2'               TO ERR-TIPO-ACESSO
              MOVE 'POSTO_BCRIO_SERVC' TO ERR-DBD-TAB
              MOVE 'SELECT  '          TO ERR-FUN-COMANDO
              MOVE  SQLCODE            TO ERR-SQL-CODE
              MOVE  '0020'             TO ERR-LOCAL
              MOVE  SPACES             TO ERR-SEGM
              PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF SQLCODE                  EQUAL ZEROS
               PERFORM 3220-OBTER-DADOS-YMKCB001
           ELSE
               MOVE 'N'                TO WRK-GRAVAR
           END-IF.

      *----------------------------------------------------------------*
       3210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ACESSO A TABELA YMKCB001                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       3220-OBTER-DADOS-YMKCB001       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE KCIMW184-AREA-COMUNICACAO.

           MOVE 01                     TO KCIMW184-E-FUNCAO.
           MOVE WRK-AGENCIA-ATU        TO KCIMW184-E-CAG-BCRIA-1.
           MOVE WRK-POSTO-ATU          TO WRK-CPAB-9-05.
           MOVE WRK-CPAB-9-03          TO KCIMW184-E-CPOSTO-BCRIO-1.
           MOVE 'KCIM0184'             TO WRK-MODULO.

           CALL WRK-MODULO             USING KCIMW184-AREA-COMUNICACAO.

           IF KCIMW184-S-COD-RETORNO  NOT EQUAL 00 AND 98
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE  '0030'             TO ERR-LOCAL
              MOVE  SPACES             TO ERR-SEGM
              PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  KCIMW184-S-COD-RETORNO EQUAL 00 AND
               KCIMW184-S-CEMPR-CORSP (1) EQUAL 10000
               MOVE 'S'                TO WRK-GRAVAR
           ELSE
               MOVE 'N'                TO WRK-GRAVAR
           END-IF.

      *----------------------------------------------------------------*
       3220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TRATAR BRAD EXPRESSO                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-TRATAR-BRAD-EXPRE          SECTION.
      *----------------------------------------------------------------*

           IF WRK-CHAVE-ATU            NOT EQUAL WRK-CHAVE-ANT
               PERFORM 3310-OBTER-DADOS-MESUB062
               MOVE WRK-CHAVE-ATU      TO WRK-CHAVE-ANT
           END-IF.

           IF WRK-GRAVAR               EQUAL 'S'
               MOVE GFCTRU-ENTRADA OF CLIEADES
                                       TO GFCTRU-ENTRADA OF CLIEPEX
               WRITE GFCTRU-ENTRADA    OF CLIEPEX
               MOVE WRK-GRAVACAO       TO WRK-OPERACAO
               PERFORM 1140-TESTAR-FS-CLIEPEX

               ADD 1                   TO ACU-GRAVADO2
           END-IF.

           PERFORM 2100-LER-CLIEADES.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ACESSO A TABELA MESUB062                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       3310-OBTER-DADOS-MESUB062       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AGENCIA-ATU        TO CDEPDC OF MESUB062.
           MOVE WRK-POSTO-ATU          TO CPOSTO-SERVC OF MESUB062.
           MOVE 9                      TO CTPO-POSTO-SERVC OF MESUB062.
           MOVE 237                    TO CEMPR-INC OF MESUB062.

           EXEC SQL
             SELECT
                   CTPO_POSTO_SERVC
             INTO
                   :MESUB062.CTPO-POSTO-SERVC
             FROM   DB2PRD.POSTO_BCRIO_SERVC
             WHERE
                   CEMPR_INC          = :MESUB062.CEMPR-INC        AND
                   CDEPDC             = :MESUB062.CDEPDC           AND
                   CPOSTO_SERVC       = :MESUB062.CPOSTO-SERVC     AND
                   CTPO_POSTO_SERVC   = :MESUB062.CTPO-POSTO-SERVC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
              MOVE 'DB2'               TO ERR-TIPO-ACESSO
              MOVE 'POSTO_BCRIO_SERVC' TO ERR-DBD-TAB
              MOVE 'SELECT  '          TO ERR-FUN-COMANDO
              MOVE  SQLCODE            TO ERR-SQL-CODE
              MOVE  '0040'             TO ERR-LOCAL
              MOVE  SPACES             TO ERR-SEGM
              PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF SQLCODE                  EQUAL ZEROS
               PERFORM 3320-OBTER-DADOS-YMKCB001
           ELSE
               MOVE 'N'                TO WRK-GRAVAR
           END-IF.

      *----------------------------------------------------------------*
       3310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ACESSO A TABELA YMKCB001                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       3320-OBTER-DADOS-YMKCB001       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE KCIMW184-AREA-COMUNICACAO.

           MOVE 01                     TO KCIMW184-E-FUNCAO.
           MOVE WRK-AGENCIA-ATU        TO KCIMW184-E-CAG-BCRIA-1.
           MOVE WRK-POSTO-ATU          TO WRK-CPAB-9-05.
           MOVE WRK-CPAB-9-03          TO KCIMW184-E-CPOSTO-BCRIO-1.
           MOVE 'KCIM0184'             TO WRK-MODULO.

           CALL WRK-MODULO             USING KCIMW184-AREA-COMUNICACAO.

           IF KCIMW184-S-COD-RETORNO  NOT EQUAL 00 AND 98
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE  '0050'             TO ERR-LOCAL
              MOVE  KCIMW184-S-COD-RETORNO TO ERR-SQL-CODE
              MOVE  'KCIM0184'         TO ERR-SEGM
              PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  KCIMW184-S-COD-RETORNO EQUAL 00 AND
               KCIMW184-S-CEMPR-CORSP (1) NOT EQUAL 10000
               MOVE 'S'                TO WRK-GRAVAR
           ELSE
               MOVE 'N'                TO WRK-GRAVAR
           END-IF.

      *----------------------------------------------------------------*
       3320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      ******************************************************************
      *  FIM DO PROCESSAMENTO                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*


           IF LKG-IDENT                EQUAL 0
               PERFORM 4100-EMITIR-DISPLAY
           ELSE
               IF LKG-IDENT            EQUAL 1
                   PERFORM 4200-EMITIR-DISPLAY
               ELSE
                   PERFORM 4300-EMITIR-DISPLAY
               END-IF
           END-IF.

           CLOSE CLIEADES
                 CLIEPAB
                 CLIEPBC
                 CLIEPEX.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.
           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  EMISSAO DE DISPLAY                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS              TO WRK-MASK.
           DISPLAY '****************** GFCT2135 ******************'.
           DISPLAY '*                                            *'.
           DISPLAY '*   TOTAL DE REG LIDOS NO ARQUIVO CLIEADES   *'.
           DISPLAY '*   REGISTROS LIDOS =    ' WRK-MASK
                                                       '         *'.
           MOVE ACU-GRAVADOS           TO WRK-MASK.
           DISPLAY '*                                            *'.
           DISPLAY '*   TOTAL REG  GRAVADOS NO ARQUIVO CLIEPAB   *'.
           DISPLAY '*   REGISTROS GRAVADOS = ' WRK-MASK
                                                       '         *'.
           DISPLAY '*                                            *'.
           DISPLAY '****************** GFCT2135 ******************'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  EMISSAO DE DISPLAY                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       4200-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS              TO WRK-MASK.
           DISPLAY '****************** GFCT2135 ******************'.
           DISPLAY '*                                            *'.
           DISPLAY '*   TOTAL DE REG LIDOS NO ARQUIVO CLIEADES   *'.
           DISPLAY '*   REGISTROS LIDOS =    ' WRK-MASK
                                                       '         *'.
           MOVE ACU-GRAVADO1           TO WRK-MASK.
           DISPLAY '*                                            *'.
           DISPLAY '*   TOTAL REG  GRAVADOS NO ARQUIVO CLIEPBC   *'.
           DISPLAY '*   REGISTROS GRAVADOS = ' WRK-MASK
                                                       '         *'.
           DISPLAY '*                                            *'.
           DISPLAY '****************** GFCT2135 ******************'.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  EMISSAO DE DISPLAY                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       4300-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS              TO WRK-MASK.
           DISPLAY '****************** GFCT2135 ******************'.
           DISPLAY '*                                            *'.
           DISPLAY '*   TOTAL DE REG LIDOS NO ARQUIVO CLIEADES   *'.
           DISPLAY '*   REGISTROS LIDOS =    ' WRK-MASK
                                                       '         *'.
           MOVE ACU-GRAVADO2           TO WRK-MASK.
           DISPLAY '*                                            *'.
           DISPLAY '*   TOTAL REG  GRAVADOS NO ARQUIVO CLIEPEX   *'.
           DISPLAY '*   REGISTROS GRAVADOS = ' WRK-MASK
                                                       '         *'.
           DISPLAY '*                                            *'.
           DISPLAY '****************** GFCT2135 ******************'.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA FORMATACAO DE ERRO                                *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 4                      TO    RETURN-CODE.
           MOVE 'GFCT2135'             TO    ERR-PGM.


           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.
           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
