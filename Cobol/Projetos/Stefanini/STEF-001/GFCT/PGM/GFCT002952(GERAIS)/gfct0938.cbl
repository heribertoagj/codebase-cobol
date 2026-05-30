      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0938.
       AUTHOR.     GIOVANE LUIZ GHIZONI.
      *================================================================*
      *                   C P M   S I S T E M A S                      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT0938                                     *
      *    PROGRAMADOR  : GIOVANE GHIZONI               - CPM/FPOLIS   *
      *    ANALISTA CPM : ALEXANDRE PEREIRA             - CPM/FPOLIS   *
      *    ANALISTA     : VANI NUNES      - PROCWORK    - GRUPO 50     *
      *    DATA         : 19/01/2007                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      EFETUA LIBERACAO DA SUSPENSAO DA TARIFA POR AGRUPAMENTO.  *
      *                                                                *
      *    BANCO DE DADOS   :                                          *
      *     DB2                                                        *
      *      TABLE                               INCLUDE/BOOK          *
      *      DB2PRD.SUSP_TARIF_AGPTO               GFCTB0E7            *
      *      DB2PRD.PARM_DATA_PROCM                GFCTB0A1            *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5523 - OBTER DESCRICAO DE MENSAGENS                   *
      *      GFCT5538 - LIBERACAO SUSP. TARIFA                         *
      *      POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                 *
      *      POOL1205 - VALIDACAO DE DATA                              *
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

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*  INICIO DA WORKING GFCT0938  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*         INDICADORES          *'.
      *----------------------------------------------------------------*

       01  IND-INDICADORES.
           05  IND-1                   PIC  9(03) COMP-3   VALUE ZEROS.
           05  IND-2                   PIC  9(03) COMP-3   VALUE ZEROS.
           05  IND-3                   PIC  9(03) COMP-3   VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*            CHAVES            *'.
      *----------------------------------------------------------------*

       01  CHV-CHAVES.
           05  CHV-ERRO-OCOR           PIC  X(01)          VALUE 'N'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  WRK-MODULO              PIC  X(08)          VALUE SPACES.
           05  WRK-DATA                PIC  9(08)          VALUE ZEROS.

           05  WRK-SQLCODE             PIC  9(09)          VALUE ZEROS.
           05  FILLER REDEFINES WRK-SQLCODE.
             10 FILLER                 PIC  X(06).
             10 WRK-SQLCODE-3          PIC  9(03).

           05  WRK-DESC-MSG-X.
             10  WRK-DESC-MSG          PIC  X(70)          VALUE SPACES.
             10  FILLER                PIC  X(09)          VALUE SPACES.

           05  WRK-DESC-MSG-G3-X.
             10  WRK-DESC-MSG-G3       PIC  X(68)          VALUE SPACES.
             10  FILLER                PIC  X(02)          VALUE SPACES.

           05  WRK-DT-DB2.
             10  WRK-DT-DB2-DIA        PIC  9(02)          VALUE ZEROS.
             10  FILLER                PIC  X(01)          VALUE SPACES.
             10  WRK-DT-DB2-MES        PIC  9(02)          VALUE ZEROS.
             10  FILLER                PIC  X(01)          VALUE SPACES.
             10  WRK-DT-DB2-ANO        PIC  9(04)          VALUE ZEROS.

           05  WRK-DDMMAAAA.
             10  WRK-DD1               PIC  9(02)          VALUE ZEROS.
             10  FILLER                PIC  X(01)          VALUE SPACES.
             10  WRK-MM1               PIC  9(02)          VALUE ZEROS.
             10  FILLER                PIC  X(01)          VALUE SPACES.
             10  WRK-AAAA1             PIC  9(04)          VALUE ZEROS.

           05  WRK-AAAAMMDD            PIC  9(08)          VALUE ZEROS.
           05  FILLER REDEFINES WRK-AAAAMMDD.
             10  WRK-AAAA2             PIC  9(04).
             10  WRK-MM2               PIC  9(02).
             10  WRK-DD2               PIC  9(02).

           05  WRK-DT-INV              PIC  9(008)         VALUE ZEROS.
           05  FILLER REDEFINES WRK-DT-INV.
             10  WRK-DT-INV-ANO        PIC  9(004).
             10  WRK-DT-INV-MES        PIC  9(002).
             10  WRK-DT-INV-DIA        PIC  9(002).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*       MENSAGENS DE ERRO      *'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGENS.
           05  WRK-MSG05.
             10  FILLER                PIC X(26) VALUE
                 'ERRO NA CHAMADA AO MODULO'.
             10  WRK-NOME-MOD          PIC X(08) VALUE SPACES.
             10  FILLER                PIC X(41) VALUE SPACES.
           05  WRK-MSG-DB2.
             10  WRK-GFCTG3-DESC-MSG   PIC X(68) VALUE SPACES.
             10  FILLER                PIC X(03) VALUE ' - '.
             10  WRK-NOME-TAB          PIC X(08) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*    AREA DO MODULO GFCT5523   *'.
      *----------------------------------------------------------------*

       COPY I#GFCTG2.
       COPY I#GFCTG3.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*         AREA DA POOL1205     *'.
      *----------------------------------------------------------------*

       01  WRK-1205-MENSAGEM           PIC  X(050)         VALUE SPACES.

       01  WRK-1205-LISTA-CAMPOS.
           03  WRK-1205-DT-ENVIADA     PIC  9(008) COMP-3  VALUE ZEROS.
           03  WRK-1205-OPCAO          PIC  X(001)         VALUE SPACES.
           03  WRK-1205-DT-JULIANA     PIC  9(007) COMP-3  VALUE ZEROS.
           03  WRK-1205-DT-EDITADA     PIC  X(010)         VALUE SPACES.
           03  WRK-1205-DT-GREGORI     PIC  9(008)         VALUE ZEROS.
           03  WRK-1205-DIA-SEMANA     PIC  X(013)         VALUE SPACES.
           03  WRK-1205-MES-EXTENSO    PIC  X(009)         VALUE SPACES.
           03  WRK-1205-DIA-UT-ANTER   PIC  9(008)         VALUE ZEROS.
           03  WRK-1205-DIA-UT-POSTE   PIC  9(008)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*     AREAS DE TABELAS DB2     *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0E7
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A1
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32) VALUE
           '*    FIM DA WORKING GFCT0938   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY I#GFCTYA.
       COPY I#GFCTYB.
       COPY I#GFCT0M.

      *================================================================*
       PROCEDURE DIVISION              USING GFCTYA-ENTRADA
                                             GFCTYB-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
      * ROTINA PRINCIPAL                                               *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 0100-INICIALIZAR.

           PERFORM 1000-PROCESSAR
           VARYING IND-1               FROM 1 BY 1
             UNTIL IND-1               GREATER GFCTYA-QTDE-OCOR.

           IF  GFCTYB-COD-MSG-ERRO     NOT EQUAL ZEROS
               PERFORM 0500-FORMATAR-CODIGO-ERRO
           END-IF.

           PERFORM 9999-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INICIALIZA AREA DE RETORNO                                  *
      *----------------------------------------------------------------*
       0100-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTYB-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCTYB-SAIDA
                                       GFCT0M-AREA-ERROS.

           PERFORM 0200-MONTAR-AREA-SAIDA.

           PERFORM 0400-CONSISTIR-DADOS.

           PERFORM 0700-OBTER-DATA-PROCESSAMENTO.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FORMATA AREAS DE RETORNO                                    *
      *----------------------------------------------------------------*
       0200-MONTAR-AREA-SAIDA          SECTION.
      *----------------------------------------------------------------*

           MOVE +1010                  TO GFCTYB-LL.
           MOVE GFCTYA-TRANSACAO       TO GFCTYB-TRANSACAO.
           MOVE GFCTYA-FUNCAO          TO GFCTYB-FUNCAO.
           MOVE GFCTYA-FILTRO          TO GFCTYB-FILTRO.
           MOVE GFCTYA-FUNC-BDSCO      TO GFCTYB-FUNC-BDSCO.
           MOVE GFCTYA-FIM             TO GFCTYB-FIM.
           MOVE '0000'                 TO GFCTG2-COD-MSG.

           PERFORM 0300-OBTER-DESCRICAO-MSG.

           MOVE GFCTG3-DESC-MSG        TO WRK-DESC-MSG.
           MOVE WRK-DESC-MSG-X         TO GFCTYB-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    OBTEM DESCRICAO DA MENSAGEM                                 *
      *----------------------------------------------------------------*
       0300-OBTER-DESCRICAO-MSG        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE                  GFCTG3-SAIDA.

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE GFCTYA-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTYA-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTYA-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTYB-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0938'         TO GFCT0M-TRANSACAO

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 9                  TO GFCTYB-ERRO

               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'GFCT0938'     TO GFCT0M-TRANSACAO
               END-IF

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE 1                  TO GFCTYB-ERRO
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO GFCTYB-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO GFCTYB-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO GFCTYB-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CONSISTE DADOS DE ENTRADA                                   *
      *----------------------------------------------------------------*
       0400-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF  GFCTYB-TRANSACAO        EQUAL SPACES OR LOW-VALUES
               MOVE  1123              TO GFCTYB-COD-MSG-ERRO
               MOVE '1123'             TO GFCTG2-COD-MSG

               PERFORM 0600-MONTAR-OCOR-AREA-SAIDA
               VARYING IND-1           FROM 1 BY 1
                 UNTIL IND-1           GREATER GFCTYA-QTDE-OCOR

               PERFORM 0500-FORMATAR-CODIGO-ERRO
           END-IF.

           IF  GFCTYB-FUNCAO           EQUAL SPACES OR LOW-VALUES
               MOVE  1124              TO GFCTYB-COD-MSG-ERRO
               MOVE '1124'             TO GFCTG2-COD-MSG

               PERFORM 0600-MONTAR-OCOR-AREA-SAIDA
               VARYING IND-1           FROM 1 BY 1
                 UNTIL IND-1           GREATER GFCTYA-QTDE-OCOR

               PERFORM 0500-FORMATAR-CODIGO-ERRO
           END-IF.

           IF  GFCTYA-AGPTO            NOT NUMERIC OR
               GFCTYA-AGPTO            EQUAL ZEROS
               MOVE  1200              TO GFCTYB-COD-MSG-ERRO
               MOVE '1200'             TO GFCTG2-COD-MSG

               PERFORM 0600-MONTAR-OCOR-AREA-SAIDA
               VARYING IND-1           FROM 1 BY 1
                 UNTIL IND-1           GREATER GFCTYA-QTDE-OCOR

               PERFORM 0500-FORMATAR-CODIGO-ERRO
           END-IF.

           IF  GFCTYA-TARIFA           NOT NUMERIC
               MOVE  1125              TO GFCTYB-COD-MSG-ERRO
               MOVE '1125'             TO GFCTG2-COD-MSG

               PERFORM 0600-MONTAR-OCOR-AREA-SAIDA
               VARYING IND-1           FROM 1 BY 1
                 UNTIL IND-1           GREATER GFCTYA-QTDE-OCOR

               PERFORM 0500-FORMATAR-CODIGO-ERRO
           END-IF.

           IF  GFCTYA-QTDE-OCOR        NOT NUMERIC OR
               GFCTYA-QTDE-OCOR        EQUAL ZEROS OR
               GFCTYA-QTDE-OCOR        GREATER 5
               MOVE  1131              TO GFCTYB-COD-MSG-ERRO
               MOVE '1131'             TO GFCTG2-COD-MSG

               PERFORM 0600-MONTAR-OCOR-AREA-SAIDA
               VARYING IND-1           FROM 1 BY 1
                 UNTIL IND-1           GREATER GFCTYA-QTDE-OCOR

               PERFORM 0500-FORMATAR-CODIGO-ERRO
           END-IF.

           IF  GFCTYA-QTDE-TOT-REG     NOT NUMERIC OR
               GFCTYA-QTDE-TOT-REG     EQUAL ZEROS
               MOVE  1132              TO GFCTYB-COD-MSG-ERRO
               MOVE '1132'             TO GFCTG2-COD-MSG

               PERFORM 0600-MONTAR-OCOR-AREA-SAIDA
               VARYING IND-1           FROM 1 BY 1
                 UNTIL IND-1           GREATER GFCTYA-QTDE-OCOR

               PERFORM 0500-FORMATAR-CODIGO-ERRO
           END-IF.

           IF  GFCTYA-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES
               MOVE  1133              TO GFCTYB-COD-MSG-ERRO
               MOVE '1133'             TO GFCTG2-COD-MSG

               PERFORM 0600-MONTAR-OCOR-AREA-SAIDA
               VARYING IND-1           FROM 1 BY 1
                 UNTIL IND-1           GREATER GFCTYA-QTDE-OCOR

               PERFORM 0500-FORMATAR-CODIGO-ERRO
           END-IF.

           IF  GFCTYA-FIM              NOT EQUAL 'S' AND 'N'
               MOVE  1134              TO GFCTYB-COD-MSG-ERRO
               MOVE '1134'             TO GFCTG2-COD-MSG

               PERFORM 0600-MONTAR-OCOR-AREA-SAIDA
               VARYING IND-1           FROM 1 BY 1
                 UNTIL IND-1           GREATER GFCTYA-QTDE-OCOR

               PERFORM 0500-FORMATAR-CODIGO-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FORMATA CODIGO DE ERRO GENERICO                             *
      *----------------------------------------------------------------*
       0500-FORMATAR-CODIGO-ERRO       SECTION.
      *----------------------------------------------------------------*

           MOVE  1                     TO GFCTYB-ERRO.
           MOVE 'S'                    TO GFCTYB-FIM.

           PERFORM 0300-OBTER-DESCRICAO-MSG.

           MOVE GFCTG3-DESC-MSG        TO WRK-DESC-MSG.
           MOVE WRK-DESC-MSG-X         TO GFCTYB-DESC-MSG-ERRO.

           PERFORM 9999-FINALIZAR.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    MONTAR AS OCORENCIA DA AREA DE SAIDA QUE SERA EXIBIDA       *
      *----------------------------------------------------------------*
       0600-MONTAR-OCOR-AREA-SAIDA     SECTION.
      *----------------------------------------------------------------*

           ADD  1                      TO GFCTYB-QTDE-TOT-REG.

           ADD  1                      TO IND-2.

           MOVE GFCTYA-CSEQ-AGPTO(IND-1)
                                       TO GFCTYB-CSEQ-AGPTO(IND-2).
           MOVE GFCTYA-COD-TARIFA(IND-1)
                                       TO GFCTYB-COD-TARIFA(IND-2).
           MOVE GFCTYA-DESC-TARIFA(IND-1)
                                       TO GFCTYB-DESC-TARIFA(IND-2).
           MOVE GFCTYA-AGENCIA(IND-1)  TO GFCTYB-AGENCIA(IND-2).
           MOVE GFCTYA-POSTO(IND-1)    TO GFCTYB-POSTO(IND-2).
           MOVE GFCTYA-CNPJ-CPF(IND-1) TO GFCTYB-CNPJ-CPF(IND-2).
           MOVE GFCTYA-FILIAL(IND-1)   TO GFCTYB-FILIAL(IND-2).
           MOVE GFCTYA-CONTROLE(IND-1) TO GFCTYB-CONTROLE(IND-2).
           MOVE GFCTYA-COD-MUNIC(IND-1)
                                       TO GFCTYB-COD-MUNIC(IND-2).
           MOVE GFCTYA-UF(IND-1)       TO GFCTYB-UF(IND-2).
           MOVE GFCTYA-DESCRICAO(IND-1)
                                       TO GFCTYB-DESCRICAO(IND-2).
           MOVE GFCTYA-DATA-FIM-SUSP(IND-1)
                                       TO GFCTYB-DATA-FIM-SUSP(IND-2).
           MOVE GFCTYA-DATA-COBR(IND-1)
                                       TO GFCTYB-DATA-COBR(IND-2).
TESTE      MOVE GFCTYA-DATA-INIC-SUSP(IND-1)
TESTE                                  TO GFCTYB-DATA-INIC-SUSP(IND-2).

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ACESSA TABELA GFCTB0A1 PARA OBTER DATA DE PROCESSAMENTO     *
      *----------------------------------------------------------------*
       0700-OBTER-DATA-PROCESSAMENTO   SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
                SELECT DPROCM_ATUAL
                INTO  :GFCTB0A1.DPROCM-ATUAL
                FROM   DB2PRD.PARM_DATA_PROCM
                WHERE  CSIST_PRINC     = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF  SQLCODE                 NOT EQUAL ZEROS  OR
               SQLWARN0                EQUAL 'W'
               MOVE '0012'             TO GFCTG2-COD-MSG

               PERFORM 0300-OBTER-DESCRICAO-MSG

               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTYB-COD-MSG-ERRO
               MOVE 'GFCTB0A1'         TO WRK-NOME-TAB

               PERFORM 0800-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FORMATA ERRO DB2 GENERICO                                   *
      *----------------------------------------------------------------*
       0800-FORMATAR-ERRO-DB2          SECTION.
      *----------------------------------------------------------------*

           MOVE 9                      TO GFCTYB-ERRO.
           MOVE 'GFCT0938'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.
           MOVE SQLCODE                TO WRK-SQLCODE
                                          GFCT0M-SQLCODE.
           MOVE WRK-SQLCODE-3          TO GFCTYB-COD-SQL-ERRO.

           PERFORM 0900-CONCATENAR-MENSAGEM.

           PERFORM 9999-FINALIZAR.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FORMATA MENSAGEM DE RETORNO                                 *
      *----------------------------------------------------------------*
       0900-CONCATENAR-MENSAGEM        SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTG3-DESC-MSG        TO WRK-DESC-MSG-G3-X.
           MOVE WRK-DESC-MSG-G3        TO WRK-GFCTG3-DESC-MSG.
           MOVE WRK-MSG-DB2            TO GFCTYB-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 1100-CONSISTIR-OCORRENCIA
              VARYING IND-3            FROM 1 BY 1
                UNTIL IND-3            GREATER GFCTYA-QTDE-OCOR.

           IF GFCTYA-CSEQ-AGPTO(IND-1) NOT EQUAL ZEROS
              IF GFCTYA-DATA-COBR(IND-1)
                                       NOT EQUAL SPACES AND LOW-VALUES
                 PERFORM 1200-CONSISTIR-DATA

                 IF CHV-ERRO-OCOR      EQUAL 'N'
                    PERFORM 1300-EFETIVAR-LIBERACAO
                 ELSE
                    MOVE 'N'           TO CHV-ERRO-OCOR
                 END-IF
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CONSISTENCIA DA OCORRENCIA                                  *
      *----------------------------------------------------------------*
       1100-CONSISTIR-OCORRENCIA       SECTION.
      *----------------------------------------------------------------*

           IF GFCTYA-CSEQ-AGPTO(IND-3) NOT NUMERIC OR
              GFCTYA-COD-TARIFA(IND-3) NOT NUMERIC OR
              GFCTYA-AGENCIA(IND-3)    NOT NUMERIC OR
              GFCTYA-POSTO(IND-3)      NOT NUMERIC OR
              GFCTYA-CNPJ-CPF(IND-3)   NOT NUMERIC OR
              GFCTYA-FILIAL(IND-3)     NOT NUMERIC OR
              GFCTYA-CONTROLE(IND-3)   NOT NUMERIC OR
              GFCTYA-COD-MUNIC(IND-3)  NOT NUMERIC
              MOVE  0001               TO GFCTYB-COD-MSG-ERRO
              MOVE '0001'              TO GFCTG2-COD-MSG

              PERFORM 0600-MONTAR-OCOR-AREA-SAIDA
              VARYING IND-1            FROM 1 BY 1
                UNTIL IND-1            GREATER GFCTYA-QTDE-OCOR

              PERFORM 0500-FORMATAR-CODIGO-ERRO
           ELSE
              IF GFCTYA-CSEQ-AGPTO(IND-3)
                                       NOT EQUAL ZEROS
                 IF GFCTYA-COD-TARIFA(IND-3)
                                       EQUAL ZEROS                 OR
                   (GFCTYA-DESC-TARIFA(IND-3)
                                       EQUAL SPACES OR LOW-VALUES) OR
                   (GFCTYA-AGPTO       NOT EQUAL 04 AND
                    GFCTYA-DESCRICAO(IND-3)
                                       EQUAL SPACES OR LOW-VALUES) OR
                   (GFCTYA-DATA-FIM-SUSP(IND-3)
                                       EQUAL SPACES OR LOW-VALUES) OR
                   (GFCTYA-DATA-COBR(IND-3)
                                       EQUAL SPACES OR LOW-VALUES) OR
                   (GFCTYA-AGPTO       EQUAL 04 AND
                    (GFCTYA-CNPJ-CPF(IND-3)
                                       EQUAL ZEROS OR
                     GFCTYA-CONTROLE(IND-3)
                                       EQUAL ZEROS))               OR
                   (GFCTYA-AGPTO       EQUAL 15 AND
                    (GFCTYA-AGENCIA(IND-3)
                                       EQUAL ZEROS OR
                     GFCTYA-POSTO(IND-3)
                                       EQUAL ZEROS))               OR
                   (GFCTYA-AGPTO       EQUAL 18 AND
                    GFCTYA-COD-MUNIC(IND-3)
                                       EQUAL ZEROS)                OR
                   (GFCTYA-AGPTO       EQUAL 19 AND
                    GFCTYA-UF(IND-3)   EQUAL SPACES OR LOW-VALUES)
                    MOVE  0001         TO GFCTYB-COD-MSG-ERRO
                    MOVE '0001'        TO GFCTG2-COD-MSG

                    PERFORM 0600-MONTAR-OCOR-AREA-SAIDA
                    VARYING IND-1      FROM 1 BY 1
                      UNTIL IND-1      GREATER GFCTYA-QTDE-OCOR

                    PERFORM 0500-FORMATAR-CODIGO-ERRO
TESTE            ELSE
TESTE               IF GFCTYA-AGPTO    EQUAL 14 AND
TESTE                  GFCTYA-AGENCIA(IND-3)
TESTE                                  EQUAL ZEROS
TESTE                  PERFORM 1110-CONSISTE-AGRUP-TOTAL
TESTE
TESTE                  IF CINDCD-AGPTO-TOT OF GFCTB0E7
TESTE                                  NOT EQUAL 'S'
TESTE                     MOVE  0001   TO GFCTYB-COD-MSG-ERRO
TESTE                     MOVE '0001'  TO GFCTG2-COD-MSG
TESTE
TESTE                     PERFORM 0600-MONTAR-OCOR-AREA-SAIDA
TESTE                     VARYING IND-1
TESTE                                  FROM 1 BY 1
TESTE                       UNTIL IND-1
TESTE                                  GREATER GFCTYA-QTDE-OCOR
TESTE
TESTE                     PERFORM 0500-FORMATAR-CODIGO-ERRO
TESTE                  END-IF
TESTE               END-IF
                 END-IF
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

TESTE *----------------------------------------------------------------*
TESTE *    OBTEM INFORMACAO DE AGRUPAMENTO TOTAL DE AGENCIA            *
TESTE *----------------------------------------------------------------*
TESTE  1110-CONSISTE-AGRUP-TOTAL       SECTION.
TESTE *----------------------------------------------------------------*
TESTE
TESTE      MOVE  GFCTYA-COD-TARIFA(IND-3)
TESTE                                  TO  CSERVC-TARIF    OF  GFCTB0E7.
TESTE      MOVE  GFCTYA-AGPTO          TO  CAGPTO-CTA      OF  GFCTB0E7.
TESTE      MOVE  GFCTYA-DATA-INIC-SUSP(IND-3)
TESTE                                  TO  DINIC-SUSP-COBR OF  GFCTB0E7.
TESTE      MOVE  GFCTYA-CSEQ-AGPTO(IND-3)
TESTE                                  TO  CSEQ-AGPTO-CTA  OF  GFCTB0E7.
TESTE
TESTE      EXEC SQL
TESTE         SELECT
TESTE                  CINDCD_AGPTO_TOT
TESTE           INTO
TESTE                 :GFCTB0E7.CINDCD-AGPTO-TOT
TESTE
TESTE           FROM   DB2PRD.SUSP_TARIF_AGPTO
TESTE
TESTE           WHERE  CSERVC_TARIF    = :GFCTB0E7.CSERVC-TARIF
TESTE           AND    CAGPTO_CTA      = :GFCTB0E7.CAGPTO-CTA
TESTE           AND    DINIC_SUSP_COBR = :GFCTB0E7.DINIC-SUSP-COBR
TESTE           AND    CSEQ_AGPTO_CTA  = :GFCTB0E7.CSEQ-AGPTO-CTA
TESTE
TESTE      END-EXEC.
TESTE
TESTE      IF (SQLCODE                 NOT EQUAL ZEROS) OR
TESTE         (SQLWARN0                EQUAL 'W'      )
TESTE          MOVE 'SUSP_TARIF_AGPTO' TO GFCT0M-NOME-TAB
TESTE          MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
TESTE          MOVE '0020'             TO GFCT0M-LOCAL
TESTE
TESTE          MOVE  0012              TO GFCTYB-COD-MSG-ERRO
TESTE          MOVE '0012'             TO GFCTG2-COD-MSG
TESTE
TESTE          MOVE 'GFCTB0E4'         TO WRK-NOME-TAB
TESTE          MOVE SQLCODE            TO WRK-SQLCODE
TESTE
TESTE          PERFORM 0800-FORMATAR-ERRO-DB2
TESTE      END-IF.
TESTE
TESTE *----------------------------------------------------------------*
TESTE  1110-99-FIM.                    EXIT.
TESTE *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    EFETUA VALIDACAO DA DATA DE COBRANCA                        *
      *----------------------------------------------------------------*
       1200-CONSISTIR-DATA             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE  WRK-1205-LISTA-CAMPOS
                       WRK-1205-MENSAGEM.

           MOVE GFCTYA-DATA-COBR(IND-1)
                                       TO WRK-DT-DB2.
           MOVE WRK-DT-DB2-DIA         TO WRK-DT-INV-DIA.
           MOVE WRK-DT-DB2-MES         TO WRK-DT-INV-MES.
           MOVE WRK-DT-DB2-ANO         TO WRK-DT-INV-ANO.
           MOVE WRK-DT-INV             TO WRK-1205-DT-ENVIADA.

           CALL 'POOL1205'             USING WRK-1205-LISTA-CAMPOS
                                             WRK-1205-MENSAGEM.

           IF  RETURN-CODE             NOT EQUAL ZEROS AND
               RETURN-CODE             NOT EQUAL 08
               MOVE 9                  TO GFCTYB-ERRO
               MOVE 'POOL1205'         TO WRK-NOME-MOD
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0938'         TO GFCT0M-PROGRAMA

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  RETURN-CODE             EQUAL 08
               PERFORM 0600-MONTAR-OCOR-AREA-SAIDA

               MOVE 'S'                TO CHV-ERRO-OCOR

               IF  GFCTYB-COD-MSG-ERRO EQUAL ZEROS
                   MOVE 1213           TO GFCTYB-COD-MSG-ERRO
                   MOVE '1213'         TO GFCTG2-COD-MSG
               END-IF

               GO TO 1200-99-FIM
           END-IF.

           IF  WRK-1205-OPCAO          NOT EQUAL 'U'
               PERFORM 0600-MONTAR-OCOR-AREA-SAIDA

               MOVE 'S'                TO CHV-ERRO-OCOR

               IF  GFCTYB-COD-MSG-ERRO EQUAL ZEROS
                   MOVE 1249           TO GFCTYB-COD-MSG-ERRO
                   MOVE '1249'         TO GFCTG2-COD-MSG
               END-IF

               GO TO 1200-99-FIM
           END-IF.

           MOVE GFCTYA-DATA-COBR(IND-1)
                                       TO WRK-DDMMAAAA.
           MOVE WRK-DD1                TO WRK-DD2.
           MOVE WRK-MM1                TO WRK-MM2.
           MOVE WRK-AAAA1              TO WRK-AAAA2.
           MOVE WRK-AAAAMMDD           TO WRK-DATA.
           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO WRK-DDMMAAAA.
           MOVE WRK-DD1                TO WRK-DD2.
           MOVE WRK-MM1                TO WRK-MM2.
           MOVE WRK-AAAA1              TO WRK-AAAA2.

           IF  WRK-DATA                NOT GREATER  WRK-AAAAMMDD
               MOVE 'S'                TO CHV-ERRO-OCOR

               IF  GFCTYB-COD-MSG-ERRO EQUAL ZEROS
                   MOVE  1207          TO GFCTYB-COD-MSG-ERRO
                   MOVE '1207'         TO GFCTG2-COD-MSG
               END-IF

               PERFORM 0600-MONTAR-OCOR-AREA-SAIDA
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    EFETUA ATUALIZACAO DA BASE PARA EFETIVAR LIBERACAO          *
      *----------------------------------------------------------------*
       1300-EFETIVAR-LIBERACAO         SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTYA-COD-TARIFA   (IND-1)
                                       TO CSERVC-TARIF      OF GFCTB0E7.
           MOVE GFCTYA-AGPTO           TO CAGPTO-CTA        OF GFCTB0E7.
           MOVE GFCTYA-CSEQ-AGPTO   (IND-1)
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0E7.
           MOVE GFCTYA-DATA-FIM-SUSP(IND-1)
                                       TO DFIM-SUSP-COBR    OF GFCTB0E7.
           MOVE 'A'                    TO CINDCD-LIBRC-COBR OF GFCTB0E7.
           MOVE GFCTYA-DATA-COBR    (IND-1)
                                       TO DCOBR-SUSP-LIBRD  OF GFCTB0E7.

           EXEC SQL
                UPDATE DB2PRD.SUSP_TARIF_AGPTO
                SET    CINDCD_LIBRC_COBR  = :GFCTB0E7.CINDCD-LIBRC-COBR,
                       DCOBR_SUSP_LIBRD   = :GFCTB0E7.DCOBR-SUSP-LIBRD,
                       HMANUT_REG         =  CURRENT TIMESTAMP
                WHERE  CSERVC_TARIF       = :GFCTB0E7.CSERVC-TARIF
                AND    CAGPTO_CTA         = :GFCTB0E7.CAGPTO-CTA
                AND    CSEQ_AGPTO_CTA     = :GFCTB0E7.CSEQ-AGPTO-CTA
                AND    DFIM_SUSP_COBR     = :GFCTB0E7.DFIM-SUSP-COBR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE '0019'             TO GFCTG2-COD-MSG

               PERFORM 0300-OBTER-DESCRICAO-MSG

               MOVE 'SUSP_TARIF_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'UPDATE'           TO GFCT0M-COMANDO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE  0019              TO GFCTYB-COD-MSG-ERRO
               MOVE 'GFCTB0E7'         TO WRK-NOME-TAB

               PERFORM 0800-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               IF  GFCTYB-COD-MSG-ERRO EQUAL ZEROS
                   MOVE  1208          TO GFCTYB-COD-MSG-ERRO
                   MOVE '1208'         TO GFCTG2-COD-MSG
               END-IF

               PERFORM 0600-MONTAR-OCOR-AREA-SAIDA
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    RETORNA AO PROGRAMA CHAMADOR                                *
      *----------------------------------------------------------------*
       9999-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE 'S'                    TO GFCTYB-FIM.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
