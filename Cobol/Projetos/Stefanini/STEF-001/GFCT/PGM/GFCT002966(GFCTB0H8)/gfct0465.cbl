      *===============================================================*
       IDENTIFICATION                  DIVISION.
      *===============================================================*

       PROGRAM-ID. GFCT0465.
       AUTHOR.     ALISON JUNIOR GHEDIN.
      *===============================================================*
      *                    C P M  -  S I S T E M A S                  *
      *---------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0465                                   *
      *    PROGRAMADOR.:   ALISON JUNIOR GHEDIN   - CPM / PATO BRANCO *
      *    ANALISTA CPM:   ADRIANO L. SANTANA     - CPM / PATO BRANCO *
      *    ANALISTA....:   RENATO TAMANAHA        - PWI / GRUPO 50    *
      *    DATA........:   22/02/2006                                 *
      *---------------------------------------------------------------*
      *    OBJETIVO....:   UNIFICACAO DE DEBITO - CONSULTA POR CNPJ/  *
      *                    CPF.                                       *
      *---------------------------------------------------------------*
      *    BCO DE DADOS:                                              *
      *                DB2                                            *
      *                TABLE                         INCLUDE/BOOK     *
      *                    DB2PRD.SERVC_TARIF_PRINC    GFCTB0D8       *
      *                    DB2PRD.TDEB_UNFCA_CLI       GFCTB0H8       *
      *---------------------------------------------------------------*
      *    INC'S.......:                                              *
      *    I#GFCTIS - CONSULTA UNIFICACAO - ENTRADA                   *
      *    I#GFCTIT - CONSULTA UNIFICACAO - SAIDA                     *
      *    I#GFCTG2 - MODULO MENSAGEM     - ENTRADA                   *
      *    I#GFCTG3 - MODULO MENSAGEM     - SAIDA                     *
      *    I#GFCTIU - TEXT PESQUISA NOME CLIENTE - ENTRADA            *
      *    I#GFCTIV - TEXT PESQUISA NOME CLIENTE - SAIDA              *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS                     *
      *---------------------------------------------------------------*
      *    MODULOS.....:                                              *
      *    POOL5523 - OBTER DESCRICAO DE MENSAGENS                    *
      *    POOL5110 - VERIFICA PESQUISA NOME CLIENTE                  *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                  *
      *===============================================================*

      *===============================================================*
       ENVIRONMENT                     DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *---------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *===============================================================*
       DATA                            DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** GFCT0465- INICIO DA AREA DE WORKING ***'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA INDEXADORES ***'.
      *---------------------------------------------------------------*

       77  IND-1                       PIC  9(002) COMP-3  VALUE ZEROS.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VARIAVEIS AUXILIARES ***'.
      *---------------------------------------------------------------*

       77  WRK-FIM-CRS                 PIC  X(001)         VALUE SPACES.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.

       01  WRK-BUSCA-CPF-CNPJ          PIC S9(009)V COMP-3 VALUE ZEROS.
       01  WRK-BUSCA-FILIAL            PIC S9(005)V COMP-3 VALUE ZEROS.
       01  WRK-BUSCA-TARIFA            PIC S9(005)V COMP-3 VALUE ZEROS.
       01  WRK-BUSCA-HINCL             PIC  X(026)         VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VARIAVEIS AUXILIARES  DE DATA ***'.
      *---------------------------------------------------------------*

       01  WRK-DATA-AUX.
           03  WRK-ANO-AUX             PIC  9(004)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  WRK-MES-AUX             PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  WRK-DIA-AUX             PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(016)         VALUE SPACES.

       01  WRK-DATA-AUX2.
           03  WRK-DIA-AUX2            PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  WRK-MES-AUX2            PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  WRK-ANO-AUX2            PIC  9(004)         VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACESSO AO MODULO GFCT5523 ***'.
      *---------------------------------------------------------------*

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACESSO AO MODULO GFCT5110 ***'.
      *---------------------------------------------------------------*

       COPY 'I#GFCTIU'.

       COPY 'I#GFCTIV'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA DB2 ***'.
      *---------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0H8
           END-EXEC.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA DECLARACAO DE CURSOR ***'.
      *---------------------------------------------------------------*

           EXEC SQL
            DECLARE CSR01-GFCTB0H8 CURSOR FOR
ST25X6*      SELECT CCGC_CPF,
ST25X6*             CFLIAL_CGC,
ST25X6       SELECT CCGC_CPF,
ST25X6              CFLIAL_CGC,
ST25X6              CCGC_CPF_ST,
ST25X6              CFLIAL_CGC_ST,
ST25X6              CSERVC_TARIF,
ST25X6              HINCL_DEB_UNIC,
ST25X6              HEXCL_DEB_UNIC
               FROM DB2PRD.TDEB_UNFCA_CLI
              WHERE
ST25X6*           CCGC_CPF           = :WRK-BUSCA-CPF-CNPJ
ST25X6*       AND CFLIAL_CGC         = :WRK-BUSCA-FILIAL
ST25X6            CCGC_CPF_ST        = :WRK-BUSCA-CPF-CNPJ
ST25X6        AND CFLIAL_CGC_ST      = :WRK-BUSCA-FILIAL
                AND HEXCL_DEB_UNIC     = :GFCTB0H8.HEXCL-DEB-UNIC
                AND ((CSERVC_TARIF     = :WRK-BUSCA-TARIFA
                AND   HINCL_DEB_UNIC  <= :WRK-BUSCA-HINCL)
                 OR  CSERVC_TARIF     >  :WRK-BUSCA-TARIFA)
           ORDER BY CSERVC_TARIF,
                    HINCL_DEB_UNIC DESC
           END-EXEC.

           EXEC SQL
            DECLARE CSR02-GFCTB0H8 CURSOR FOR
ST25X6*      SELECT CCGC_CPF,
ST25X6*             CFLIAL_CGC,
ST25X6       SELECT CCGC_CPF,
ST25X6              CFLIAL_CGC,
ST25X6              CCGC_CPF_ST,
ST25X6              CFLIAL_CGC_ST,
ST25X6              CSERVC_TARIF,
ST25X6              HINCL_DEB_UNIC,
ST25X6              HEXCL_DEB_UNIC
               FROM DB2PRD.TDEB_UNFCA_CLI
              WHERE
ST25X6*           CCGC_CPF           = :WRK-BUSCA-CPF-CNPJ
ST25X6*       AND CFLIAL_CGC         = :WRK-BUSCA-FILIAL
ST25X6            CCGC_CPF_ST        = :WRK-BUSCA-CPF-CNPJ
ST25X6        AND CFLIAL_CGC_ST      = :WRK-BUSCA-FILIAL
                AND ((CSERVC_TARIF     = :WRK-BUSCA-TARIFA
                AND   HINCL_DEB_UNIC  <= :WRK-BUSCA-HINCL)
                 OR  CSERVC_TARIF     >  :WRK-BUSCA-TARIFA)
           ORDER BY CSERVC_TARIF,
                    HINCL_DEB_UNIC DESC
           END-EXEC.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** GFCT0465 - FIM DA AREA DE WORKING ***'.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTIS'.

       COPY 'I#GFCTIT'.

       COPY 'I#GFCT0M'.

      *===============================================================*
       PROCEDURE DIVISION              USING GFCTIS-ENTRADA
                                             GFCTIT-SAIDA
                                             GFCT0M-AREA-ERROS.
      *===============================================================*

      *****************************************************************
      * ROTINA PRINCIPAL                                              *
      *****************************************************************
      *---------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *---------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 3000-FINALIZAR.

      *---------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *****************************************************************
      * ROTINA PARA INICIALIZACAO DO PROGRAMA                         *
      *****************************************************************
      *---------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *---------------------------------------------------------------*

           MOVE SPACES                 TO GFCTIT-SAIDA
                                          GFCTG3-SAIDA
                                          GFCTG2-ENTRADA
                                          GFCTIU-ENTRADA
                                          GFCTIV-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                     GFCTIT-SAIDA
                                          GFCTG3-SAIDA
                                          GFCTG2-ENTRADA
                                          GFCTIU-ENTRADA
                                          GFCTIV-SAIDA
                                          GFCT0M-AREA-ERROS.

           PERFORM 1100-MONTAR-AREA-SAIDA.

           PERFORM 1200-CONSISTIR-CAMPOS.

      *---------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *****************************************************************
      * ROTINA PARA MONTAGEM DA AREA DE SAIDA A PARTIR DA AREA DE EN- *
      * TRADA                                                         *
      *****************************************************************
      *---------------------------------------------------------------*
       1100-MONTAR-AREA-SAIDA          SECTION.
      *---------------------------------------------------------------*

           MOVE +362                   TO GFCTIT-LL.
           MOVE GFCTIS-TRANSACAO       TO GFCTIT-TRANSACAO.
           MOVE GFCTIS-FUNCAO          TO GFCTIT-FUNCAO.
           MOVE GFCTIS-FILTRO          TO GFCTIT-FILTRO.
           MOVE GFCTIS-NOME            TO GFCTIT-NOME.

           IF  GFCTIS-QTDE-OCOR        NOT NUMERIC OR
               GFCTIS-QTDE-OCOR        EQUAL ZEROS
               MOVE ZEROS              TO GFCTIT-QTDE-OCOR
           ELSE
               MOVE GFCTIS-QTDE-OCOR   TO GFCTIT-QTDE-OCOR
           END-IF.

           IF  GFCTIS-QTDE-TOT-REG     NOT NUMERIC OR
               GFCTIS-QTDE-TOT-REG     EQUAL ZEROS
               MOVE ZEROS              TO GFCTIT-QTDE-TOT-REG
           ELSE
               MOVE GFCTIS-QTDE-TOT-REG TO GFCTIT-QTDE-TOT-REG
           END-IF.

           MOVE GFCTIS-FUNC-BDSCO      TO GFCTIT-FUNC-BDSCO.
           MOVE GFCTIS-PONTEIRO        TO GFCTIT-PONTEIRO.

           IF  GFCTIS-DPTO             NOT NUMERIC OR
               GFCTIS-DPTO             EQUAL ZEROS
               MOVE ZEROS              TO GFCTIT-DPTO
           ELSE
               MOVE GFCTIS-DPTO        TO GFCTIT-DPTO
           END-IF.

           IF  GFCTIS-MASTER           EQUAL SPACES OR
               GFCTIS-MASTER           EQUAL LOW-VALUES
               MOVE SPACES             TO GFCTIT-MASTER
           ELSE
               MOVE GFCTIS-MASTER      TO GFCTIT-MASTER
           END-IF.

           MOVE GFCTIS-TERMINAL        TO GFCTIT-TERMINAL.
           MOVE GFCTIS-ACAO            TO GFCTIT-ACAO.

           MOVE ZEROS                  TO GFCTIT-ERRO
                                          GFCTIT-COD-SQL-ERRO
                                          GFCTIT-COD-MSG-ERRO
                                          GFCTG2-COD-MSG.

           PERFORM 1110-OBTER-DESCRICAO-MSG.

           MOVE GFCTG3-DESC-MSG        TO GFCTIT-DESC-MSG-ERRO.

           MOVE 'N'                    TO GFCTIT-FIM
                                          WRK-FIM-CRS.

      *---------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *****************************************************************
      * ROTINA PARA ACESSO AO MODULO DE DESCRICAO DE MENSAGENS        *
      *****************************************************************
      *---------------------------------------------------------------*
       1110-OBTER-DESCRICAO-MSG        SECTION.
      *---------------------------------------------------------------*

           MOVE 100                    TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE GFCTIS-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTIS-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTIS-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  GFCTG3-ERRO             NOT EQUAL ZEROS
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE GFCTG3-DESC-MSG-ERRO TO GFCT0M-TEXTO
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
               ELSE
                   MOVE GFCTG3-COD-SQL-ERRO TO GFCTIT-COD-SQL-ERRO
               END-IF
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'GFCT0465'         TO GFCT0M-TRANSACAO
               MOVE GFCTG3-ERRO        TO GFCTIT-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *---------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *****************************************************************
      * ROTINA PARA CONSISTENCIA DE CAMPOS                            *
      *****************************************************************
      *---------------------------------------------------------------*
       1200-CONSISTIR-CAMPOS           SECTION.
      *---------------------------------------------------------------*

           IF (GFCTIS-TRANSACAO        EQUAL SPACES OR
                                       LOW-VALUES)  OR
              (GFCTIS-FUNCAO           EQUAL SPACES OR
                                       LOW-VALUES)  OR
              (GFCTIS-FUNC-BDSCO       NOT NUMERIC  OR
               GFCTIS-FUNC-BDSCO       EQUAL ZEROS) OR
ST2506*       (GFCTIS-CPF-CNPJ         NOT NUMERIC) OR
ST2506*       (GFCTIS-FILIAL           NOT NUMERIC) OR
ST2506        (GFCTIS-CPF-CNPJ         EQUAL SPACES OR
ST2506         GFCTIS-CPF-CNPJ         EQUAL LOW-VALUES) OR
ST2506        (GFCTIS-FILIAL           EQUAL SPACES OR
ST2506         GFCTIS-FILIAL           EQUAL LOW-VALUES) OR
              (GFCTIS-CTRL             NOT NUMERIC) OR
              (GFCTIS-QTDE-OCOR        NOT NUMERIC) OR
              (GFCTIS-QTDE-TOT-REG     NOT NUMERIC) OR
              (GFCTIS-CPF-CNPJ-P       NOT NUMERIC) OR
              (GFCTIS-FILIAL-P         NOT NUMERIC) OR
              (GFCTIS-TARIFA-P         NOT NUMERIC) OR
              (GFCTIS-ACAO             NOT EQUAL 'A' AND 'C')
               MOVE 'S'                TO GFCTIT-FIM

               MOVE 1                  TO GFCTIT-ERRO
               MOVE 0001               TO GFCTIT-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE ZEROS              TO GFCTIT-COD-SQL-ERRO
               PERFORM 1110-OBTER-DESCRICAO-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTIT-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *---------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *****************************************************************
      * ROTINA DE PROCESSAMENTO PRINCIPAL DO PROGRAMA                 *
      *****************************************************************
      *---------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *---------------------------------------------------------------*

           PERFORM 2100-VALIDAR-CPF-CNPJ.

           PERFORM 2200-MONTAR-GRADE.

      *---------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *****************************************************************
      * ROTINA PARA VALIDACAO DE CPF E CNPJ                           *
      *****************************************************************
      *---------------------------------------------------------------*
       2100-VALIDAR-CPF-CNPJ           SECTION.
      *---------------------------------------------------------------*

           MOVE GFCTIS-TRANSACAO       TO GFCTIU-TRANSACAO.
           MOVE GFCTIS-FUNCAO          TO GFCTIU-FUNCAO.
           MOVE 'N'                    TO GFCTIU-FIM.
           MOVE GFCTIS-FUNC-BDSCO      TO GFCTIU-FUNC-BDSCO.
ST25X6*    MOVE GFCTIS-CPF-CNPJ        TO GFCTIU-NRO-CPF-CNPJ.
ST25X6*    MOVE GFCTIS-FILIAL          TO GFCTIU-NRO-FILIAL.
ST25X6     MOVE GFCTIS-CPF-CNPJ        TO GFCTIU-COD-CPF-CNPJ.
ST25X6     MOVE GFCTIS-FILIAL          TO GFCTIU-COD-FILIAL.
           MOVE GFCTIS-CTRL            TO GFCTIU-CTRL-CPF-CNPJ.

           MOVE 'GFCT5110'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTIU-ENTRADA
                                             GFCTIV-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  GFCTIV-ERRO             NOT EQUAL ZEROS AND 1
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE GFCTIV-DESC-MSG-ERRO TO GFCT0M-TEXTO
               ELSE
                   MOVE GFCTIV-COD-SQL-ERRO TO GFCTIT-COD-SQL-ERRO
               END-IF
               MOVE GFCTIV-ERRO        TO GFCTIT-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'GFCT0465'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTIV-ERRO             EQUAL 1
               MOVE 'S'                TO GFCTIT-FIM

               MOVE 1                  TO GFCTIT-ERRO
               MOVE GFCTIV-COD-MSG-ERRO  TO GFCTIT-COD-MSG-ERRO
               MOVE ZEROS              TO GFCTIT-COD-SQL-ERRO
               MOVE GFCTIV-DESC-MSG-ERRO TO GFCTIT-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE GFCTIV-NOME-CLIENTE    TO GFCTIT-NOME.

      *---------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *****************************************************************
      * ROTINA DE MONTAGEM DA AREA DE SAIDA                           *
      *****************************************************************
      *---------------------------------------------------------------*
       2200-MONTAR-GRADE               SECTION.
      *---------------------------------------------------------------*

           PERFORM 2210-ABRIR-CURSOR.

           PERFORM 2220-LER-CURSOR.

           IF  WRK-FIM-CRS             EQUAL 'S'
               MOVE 'S'                TO GFCTIT-FIM

               MOVE 1                  TO GFCTIT-ERRO
               MOVE ZEROS              TO GFCTIT-COD-SQL-ERRO
               MOVE 0797               TO GFCTIT-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESCRICAO-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTIT-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2230-PROCESSAR-CURSOR VARYING
               IND-1 FROM 1 BY 1       UNTIL
               IND-1                   GREATER 6 OR
               WRK-FIM-CRS             EQUAL 'S'.

           IF  IND-1                   GREATER 6 AND
               WRK-FIM-CRS             NOT EQUAL 'S'
               MOVE CCGC-CPF OF GFCTB0H8
                                       TO GFCTIT-CPF-CNPJ-P
               MOVE CFLIAL-CGC OF GFCTB0H8
                                       TO GFCTIT-FILIAL-P
               MOVE CSERVC-TARIF OF GFCTB0H8
                                       TO GFCTIT-TARIFA-P
               MOVE HINCL-DEB-UNIC OF GFCTB0H8
                                       TO GFCTIT-TIMESTAMP
           END-IF.

           PERFORM 2240-FECHAR-CURSOR.

      *---------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *****************************************************************
      * ROTINA PARA ABRIR O CURSOR                                    *
      *****************************************************************
      *---------------------------------------------------------------*
       2210-ABRIR-CURSOR               SECTION.
      *---------------------------------------------------------------*

           IF  GFCTIS-CPF-CNPJ-P       EQUAL ZEROS AND
               GFCTIS-FILIAL-P         EQUAL ZEROS
               MOVE GFCTIS-CPF-CNPJ    TO WRK-BUSCA-CPF-CNPJ
               MOVE GFCTIS-FILIAL      TO WRK-BUSCA-FILIAL
           ELSE
               MOVE GFCTIS-CPF-CNPJ-P  TO WRK-BUSCA-CPF-CNPJ
               MOVE GFCTIS-FILIAL-P    TO WRK-BUSCA-FILIAL
           END-IF.

           MOVE GFCTIS-TARIFA-P        TO WRK-BUSCA-TARIFA.

           IF  GFCTIS-TIMESTAMP        EQUAL SPACES OR LOW-VALUES
               MOVE '0001-01-01-01.01.01.000001'
                                       TO WRK-BUSCA-HINCL
           ELSE
               MOVE GFCTIS-TIMESTAMP   TO WRK-BUSCA-HINCL
           END-IF.

           IF  GFCTIS-ACAO             EQUAL 'A'
               MOVE '0001-01-01-01.01.01.000001'
                                       TO HEXCL-DEB-UNIC OF GFCTB0H8
               EXEC SQL
                   OPEN CSR01-GFCTB0H8
               END-EXEC
           ELSE
               EXEC SQL
                   OPEN CSR02-GFCTB0H8
               END-EXEC
           END-IF.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'GFCT0465'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TDEB_UNFCA_CLI  ' TO GFCT0M-NOME-TAB
               MOVE 'OPEN    '         TO GFCT0M-COMANDO-SQL
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTIT-ERRO
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTIT-COD-SQL-ERRO
               MOVE  0005              TO GFCTIT-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESCRICAO-MSG
               STRING GFCTG3-DESC-MSG
                      ' - GFCTB0H8'
               DELIMITED BY '  ' INTO GFCTIT-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *---------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *****************************************************************
      * ROTINA DE LEITURA DO CURSOR                                   *
      *****************************************************************
      *---------------------------------------------------------------*
       2220-LER-CURSOR                 SECTION.
      *---------------------------------------------------------------*

           IF  GFCTIS-ACAO             EQUAL 'A'
               EXEC SQL
                   FETCH CSR01-GFCTB0H8
                    INTO :GFCTB0H8.CCGC-CPF,
                         :GFCTB0H8.CFLIAL-CGC,
ST25X6                   :GFCTB0H8.CCGC-CPF-ST,
ST25X6                   :GFCTB0H8.CFLIAL-CGC-ST,
                         :GFCTB0H8.CSERVC-TARIF,
                         :GFCTB0H8.HINCL-DEB-UNIC,
                         :GFCTB0H8.HEXCL-DEB-UNIC
               END-EXEC
           ELSE
               EXEC SQL
                   FETCH CSR02-GFCTB0H8
                    INTO :GFCTB0H8.CCGC-CPF,
                         :GFCTB0H8.CFLIAL-CGC,
ST25X6                   :GFCTB0H8.CCGC-CPF-ST,
ST25X6                   :GFCTB0H8.CFLIAL-CGC-ST,
                         :GFCTB0H8.CSERVC-TARIF,
                         :GFCTB0H8.HINCL-DEB-UNIC,
                         :GFCTB0H8.HEXCL-DEB-UNIC
               END-EXEC
           END-IF.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'GFCT0465'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TDEB_UNFCA_CLI  ' TO GFCT0M-NOME-TAB
               MOVE 'FETCH   '         TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTIT-ERRO
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTIT-COD-SQL-ERRO
               MOVE  0006              TO GFCTIT-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESCRICAO-MSG
               STRING GFCTG3-DESC-MSG
                      ' - GFCTB0H8'
               DELIMITED BY '  ' INTO GFCTIT-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CRS
                                          GFCTIT-FIM
           END-IF.

      *---------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *****************************************************************
      * ROTINA DE MOVIMENTACAO DE CAMPOS LIDOS NO CURSOR PARA A AREA  *
      * DE SAIDA                                                      *
      *****************************************************************
      *---------------------------------------------------------------*
       2230-PROCESSAR-CURSOR           SECTION.
      *---------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0H8
                                       TO GFCTIT-TARIFA(IND-1).
           MOVE HINCL-DEB-UNIC         OF GFCTB0H8
                                       TO WRK-DATA-AUX
           MOVE WRK-DIA-AUX            TO WRK-DIA-AUX2
           MOVE WRK-MES-AUX            TO WRK-MES-AUX2
           MOVE WRK-ANO-AUX            TO WRK-ANO-AUX2
           MOVE WRK-DATA-AUX2          TO GFCTIT-DT-INI(IND-1).

           IF  HEXCL-DEB-UNIC          OF GFCTB0H8 EQUAL
                                       '0001-01-01-01.01.01.000001'
               MOVE 1                  TO GFCTIT-SIT(IND-1)
               MOVE SPACES             TO GFCTIT-DT-FIM(IND-1)
           ELSE
               MOVE 2                  TO GFCTIT-SIT(IND-1)
               MOVE HEXCL-DEB-UNIC     OF GFCTB0H8
                                       TO WRK-DATA-AUX
               MOVE WRK-DIA-AUX        TO WRK-DIA-AUX2
               MOVE WRK-MES-AUX        TO WRK-MES-AUX2
               MOVE WRK-ANO-AUX        TO WRK-ANO-AUX2
               MOVE WRK-DATA-AUX2      TO GFCTIT-DT-FIM(IND-1)
           END-IF.

           MOVE HINCL-DEB-UNIC         OF GFCTB0H8
                                       TO
                                         GFCTIT-DT-INI-TIMESTAMP(IND-1).
           MOVE HEXCL-DEB-UNIC         OF GFCTB0H8
                                       TO
                                         GFCTIT-DT-FIM-TIMESTAMP(IND-1).

           PERFORM 2231-OBTER-DESC-TARIFA.

           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO GFCTIT-DESC-TARIFA(IND-1).

           ADD +108                    TO GFCTIT-LL.
           ADD 1                       TO GFCTIT-QTDE-TOT-REG.

           PERFORM 2220-LER-CURSOR.

      *---------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *****************************************************************
      * ROTINA PARA OBTER DESCRICAO DE TARIFA                         *
      *****************************************************************
      *---------------------------------------------------------------*
       2231-OBTER-DESC-TARIFA          SECTION.
      *---------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0H8 TO CSERVC-TARIF OF GFCTB0D8.

           EXEC SQL
             SELECT RSERVC_TARIF_REDZD
               INTO :GFCTB0D8.RSERVC-TARIF-REDZD
               FROM DB2PRD.SERVC_TARIF_PRINC
              WHERE CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'GFCT0465'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'SERVC_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTIT-ERRO
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTIT-COD-SQL-ERRO
               MOVE  0011              TO GFCTIT-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESCRICAO-MSG
               STRING GFCTG3-DESC-MSG
                      ' - GFCTB0D8'
               DELIMITED BY '  ' INTO GFCTIT-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE SPACES             TO RSERVC-TARIF-REDZD OF GFCTB0D8
           END-IF.

      *---------------------------------------------------------------*
       2231-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *****************************************************************
      * ROTINA PARA FECHAR O CURSOR                                   *
      *****************************************************************
      *---------------------------------------------------------------*
       2240-FECHAR-CURSOR              SECTION.
      *---------------------------------------------------------------*

           IF  GFCTIS-ACAO             EQUAL 'A'
               EXEC SQL
                  CLOSE CSR01-GFCTB0H8
               END-EXEC
           ELSE
               EXEC SQL
                  CLOSE CSR02-GFCTB0H8
               END-EXEC
           END-IF.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'GFCT0465'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TDEB_UNFCA_CLI  ' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE   '         TO GFCT0M-COMANDO-SQL
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTIT-ERRO
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTIT-COD-SQL-ERRO
               MOVE  0011              TO GFCTIT-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESCRICAO-MSG
               STRING GFCTG3-DESC-MSG
                      ' - GFCTB0H8'
               DELIMITED BY '  ' INTO GFCTIT-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *---------------------------------------------------------------*
       2240-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *****************************************************************
      * ROTINA PARA FINALIZACAO DO PROGRAMA                           *
      *****************************************************************
      *---------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *---------------------------------------------------------------*

           GOBACK.

      *---------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
