      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0335.

      *================================================================*
      *                  SONDA PROCWORK INFORMATICA                    *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA.........: GFCT0335                                 *
      *    PROGRAMADOR......: FABRICA SOFTWARE        - SONDA PROCWORK *
      *                                                                *
      *    ANALISTA.........: MARCUS VINICIUS                          *
      *    LIDER PROJETO....: RODRIGO / MARCELO TEODORO                *
      *    GERENCIA / EQUIPE: GERENCIA-E / EQUIPE-1   - SONDA PROCWORK *
      *                                                                *
      *    GRUPO NEGOCIO....: DDS-GP50                                 *
      *    ANALISTA.........: HELENA                                   *
      *                                                                *
      *    DATA.............: FEV/2010                                 *
      *                                                                *
      *    PROJETO..........: ADESAO DE CESTAS CELULAR                 *
      *                                                                *
      *    OBJETIVO.........: GERAR LISTA DE CONSULTA DE CELULARES     *
      *                       ADERIDOS.                                *
      *                                                                *
      *    BANCO DE DADOS...:                                          *
      *      DB2                                                       *
      *      TABLE                             INCLUDE/BOOK            *
      *        DB2PRD.ADSAO_INDVD_PCOTE          GFCTB009              *
      *        DB2PRD.TADSAO_INDVD_CLULR         GFCTB0M2              *
      *        DB2PRD.SERVC_TARIF_PRINC          GFCTB0D8              *
      *                                                                *
      *    BOOK'S...........:                                          *
      *        GFCTWAMK - ENT                                          *
      *        GFCTWAML - SAI                                          *
      *        I#GFCTFZ - ENT - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *        I#GFCTG1 - SAI - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *        I#GFCTG2 - ENT - OBTER DESCRICAO DE MENSAGEM.           *
      *        I#GFCTG3 - SAI - OBTER DESCRICAO DE MENSAGEM.           *
      *        I#GFCT0M - AREA DE COMUNICACAO - ERROS.                 *
      *                                                                *
      *    MODULOS CHAMADOS.:                                          *
      *        GFCT5522 - VERIFICAR DISPONIBILIDADE DO ON-LINE.        *
      *        GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                *
      *        GFCT5054 - OBTER DADOS DO CLIENTE.                      *
      *                                                                *
      *================================================================*
VI0710*      S O N D A   P R O C W O R K   O U T S O U R C I N G       *
VI0710*----------------------------------------------------------------*
VI0710*    ULTIMA ALTERACAO EM MAI/2010 - FABRICA                      *
VI0710*    - RETIRADA DO PROCESSO DE VERIFICACAO DE CFUNC-MANUT DA     *
VI0710*      BASE GFCTB0M2 IGUAL A ZEROS PARA SITUACAO 'CANCELADA'     *
VI0710*                                                                *
VI0710*================================================================*
022012*----------------------------------------------------------------*
022012*                 U L T I M A   A L T E R A C A O                *
022012*----------------------------------------------------------------*
022012*        SONDA PROCWORK - CONSULTORIA - ALTERACAO                *
022012*----------------------------------------------------------------*
022012*                                                                *
022012*    PROGRAMADOR.:  FABRICA         - SONDA/PROCWORK             *
022012*    ANALISTA....:  PAGNOCCA        - SONDA/PROCWORK             *
022012*    DATA........:  FEV / 2012                                   *
022012*    OBJETIVO....:  PROJETO CELULAR 09 DIGITOS.                  *
022012*                                                                *
BI0216*----------------------------------------------------------------*
BI0216*                 U L T I M A   A L T E R A C A O                *
BI0216*----------------------------------------------------------------*
BI0216*        SONDA IT  - ALTERACAO - BI0216                          *
BI0216*----------------------------------------------------------------*
BI0216*                                                                *
BI0216*    ANALISTA....:  UBIRAJARA       - SONDA IT                   *
BI0216*    DATA........:  02 / 2016                                    *
BI0216*    OBJETIVO....:  HSBC - INIBIR EMISSAO DE MENSAGEM 1428       *
BI0216*                          AGENCIA MAIOR QUE 4000.               *
BI0216*                                                                *
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
       77  FILLER                      PIC  X(050)         VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA INDEXADORES *'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(003) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREAS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-FIM-CURSOR              PIC  X(001)         VALUE SPACES.
       77  WRK-NOM-TAB                 PIC  X(018)         VALUE SPACES.
       77  WRK-INC-TAB                 PIC  X(008)         VALUE SPACES.
       77  WRK-COMANDO-SQL             PIC  X(010)         VALUE SPACES.
       77  WRK-LOCAL                   PIC  X(004)         VALUE SPACES.

       01  WRK-QTDE-TOT-REG            PIC  9(006) COMP-3  VALUE ZEROS.

       01  WRK-MSG01.
           05  FILLER                  PIC  X(026)         VALUE
               'ERRO NA CHAMADA DO MODULO'.
           05  WRK-NOME-MOD            PIC  X(008)         VALUE SPACES.

       01  WRK-TAM-03-COM-S            PIC -9(003)         VALUE ZEROS.
       01  FILLER REDEFINES WRK-TAM-03-COM-S.
           05  FILLER                  PIC  X(001).
           05  WRK-TAM-03-SEM-S        PIC  9(003).

       01  WRK-TAM-05-COM-S            PIC -9(005)         VALUE ZEROS.
       01  FILLER REDEFINES WRK-TAM-05-COM-S.
           05  FILLER                  PIC  X(001).
           05  WRK-TAM-05-SEM-S        PIC  9(005).

022012 01  WRK-TAM-11-COM-S            PIC -9(011)         VALUE ZEROS.
022012 01  FILLER REDEFINES WRK-TAM-11-COM-S.
           05  FILLER                  PIC  X(001).
022012     05  WRK-TAM-11-SEM-S        PIC  9(011).

       01  WRK-TAM-09-COM-S            PIC -9(009)         VALUE ZEROS.
       01  FILLER REDEFINES WRK-TAM-09-COM-S.
           05  FILLER                  PIC  X(001).
           05  WRK-TAM-09-SEM-S        PIC  9(009).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREAS DE COMUNICACAO COM GFCT5522 *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTFZ'.

       COPY 'I#GFCTG1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREAS DE COMUNICACAO COM GFCT5523 *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE ACESSO AO MODULO GFCT5054'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG0'.

       COPY 'I#GFCTHA'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB009
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0M2
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0D8
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE DECLARACAO DE CURSOR *'.
      *----------------------------------------------------------------*

           EXEC SQL
                  DECLARE CSR01-GFCTB0M2 CURSOR FOR
                   SELECT
                          CSERVC_TARIF,
                          HINCL_REG,
                          NSEQ_REG_CLULR,
                          COPER_FONE_MOVEL,
                          CDDD_FONE_MOVEL,
022012                    NLIN_TFONI,
                          CFUNC_MANUT
                     FROM DB2PRD.TADSAO_INDVD_CLULR
                    WHERE
                          CAG_BCRIA        = :GFCTB0M2.CAG-BCRIA
                      AND CCTA_CLI         = :GFCTB0M2.CCTA-CLI
                     AND ((HINCL_REG       = :GFCTB0M2.HINCL-REG
                      AND NSEQ_REG_CLULR  <= :GFCTB0M2.NSEQ-REG-CLULR)
                      OR  (HINCL_REG       < :GFCTB0M2.HINCL-REG))

                 ORDER BY HINCL_REG       DESC,
                          NSEQ_REG_CLULR  DESC
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'GFCTWAMK'.

       COPY 'GFCTWAML'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE DIVISION              USING GFCTWAMK-ENTRADA
                                             GFCTWAML-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *ROTINA PARA INICIAR PROCESSAMENTO                               *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA INICIALIZAR CAMPOS                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          GFCTWAML-SAIDA.

           INITIALIZE GFCT0M-AREA-ERROS
                      GFCTWAML-SAIDA.

           PERFORM 1100-VERIFICAR-SIST-DISPONIVEL.

           PERFORM 1200-MONTAR-SAIDA.

           PERFORM 1300-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA VERIFICAR SISTEMA DISPONIVEL                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-VERIFICAR-SIST-DISPONIVEL  SECTION.
      *----------------------------------------------------------------*

           INITIALIZE GFCTG1-SAIDA.

           MOVE +100                   TO GFCTFZ-LL.
           MOVE ZEROS                  TO GFCTFZ-ZZ.
           MOVE WAMK-TRANSACAO         TO GFCTFZ-TRANSACAO.
           MOVE WAMK-FUNCAO            TO GFCTFZ-FUNCAO.
           MOVE WAMK-FUNC-BDSCO        TO GFCTFZ-FUNC-BDSCO.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE 'N'                    TO GFCTFZ-FIM.

           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO WAML-ERRO
               MOVE WRK-MODULO         TO WRK-NOME-MOD
                                          GFCT0M-PROGRAMA
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0335'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 9
               MOVE 9                  TO WAML-ERRO
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'GFCT0335'     TO GFCT0M-TRANSACAO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 9                  TO WAML-ERRO
               MOVE 'SISTEMA INDISPONIVEL'
                                       TO GFCT0M-TEXTO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0335'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA INICIALIZAR SAIDA                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-MONTAR-SAIDA               SECTION.
      *----------------------------------------------------------------*

           MOVE +275                   TO WAML-LL.
           MOVE ZEROS                  TO WAML-ZZ.
           MOVE WAMK-TRANSACAO         TO WAML-TRANSACAO.
           MOVE WAMK-FUNCAO            TO WAML-FUNCAO.
           MOVE WAMK-FILTRO            TO WAML-FILTRO.
           MOVE WAMK-PONTEIRO          TO WAML-PONTEIRO.
           MOVE WAMK-QTDE-OCOR         TO WAML-QTDE-OCOR.
           MOVE WAMK-QTDE-TOT-REG      TO WAML-QTDE-TOT-REG.
           MOVE WAMK-FUNC-BDSCO        TO WAML-FUNC-BDSCO.

           MOVE 'N'                    TO WAML-FIM.

           MOVE ZEROS                  TO WAML-COD-SQL-ERRO
                                          WAML-COD-MSG-ERRO
                                          WAML-ERRO.

           MOVE '0000'                 TO GFCTG2-COD-MSG.

           PERFORM 1310-OBTER-DESC-MENSAGEM.

           MOVE GFCTG3-DESC-MSG        TO WAML-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR DADOS                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF  WAMK-TRANSACAO        EQUAL LOW-VALUES   OR
               WAMK-TRANSACAO        EQUAL SPACES       OR
               WAMK-FUNCAO           EQUAL LOW-VALUES   OR
               WAMK-FUNCAO           EQUAL SPACES       OR
               WAMK-FUNC-BDSCO       EQUAL LOW-VALUES   OR
               WAMK-FUNC-BDSCO       EQUAL SPACES       OR
               WAMK-FILTRO-AGENCIA   NOT NUMERIC        OR
               WAMK-FILTRO-AGENCIA   EQUAL ZEROS        OR
               WAMK-FILTRO-CONTA     NOT NUMERIC        OR
               WAMK-FILTRO-CONTA     EQUAL ZEROS        OR
               WAMK-PONTEIRO-SEQUENCIA NOT NUMERIC      OR
               WAMK-QTDE-OCOR        NOT NUMERIC        OR
               WAMK-QTDE-TOT-REG     NOT NUMERIC        OR
               WAMK-FIM              NOT EQUAL 'S' AND 'N'
               MOVE 'S'                TO WAML-FIM
               MOVE 1                  TO WAML-ERRO
               MOVE ZEROS              TO WAML-COD-SQL-ERRO
               MOVE 0001               TO WAML-COD-MSG-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO WAML-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA OBTER DESCRICAO DA MENSAGEM                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1310-OBTER-DESC-MENSAGEM        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                         TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE 100                            TO GFCTG2-LL.
           MOVE ZEROS                          TO GFCTG2-ZZ.
           MOVE WAMK-TRANSACAO               TO GFCTG2-TRANSACAO.
           MOVE WAMK-FUNCAO                  TO GFCTG2-FUNCAO.
           MOVE WAMK-FUNC-BDSCO              TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                            TO GFCTG2-TIPO-PROC.
           MOVE 'N'                            TO GFCTG2-FIM.

           MOVE 'GFCT5523'                     TO WRK-MODULO.

           CALL WRK-MODULO                     USING GFCTG2-ENTRADA
                                                     GFCTG3-SAIDA
                                                     GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                     NOT EQUAL ZEROS
               MOVE 9                          TO WAML-ERRO
               MOVE WRK-MODULO                 TO GFCT0M-PROGRAMA
                                                  WRK-NOME-MOD
               MOVE WRK-MSG01                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0335'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO                     EQUAL 9
               MOVE GFCTG3-ERRO                TO WAML-ERRO
               IF  GFCT0M-TIPO-ACESSO          EQUAL 'DB2'
                   MOVE GFCTG3-COD-SQL-ERRO    TO WAML-COD-SQL-ERRO
                   MOVE GFCTG3-COD-MSG-ERRO    TO WAML-COD-MSG-ERRO
                   MOVE GFCTG3-DESC-MSG-ERRO   TO WAML-DESC-MSG-ERRO
               ELSE
                   MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
                   MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
                   MOVE GFCTG3-DESC-MSG-ERRO(1:75)
                                               TO GFCT0M-TEXTO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO                     EQUAL 1
               MOVE 'S'                        TO WAML-FIM
               MOVE GFCTG3-ERRO                TO WAML-ERRO
               MOVE GFCTG3-COD-SQL-ERRO        TO WAML-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO        TO WAML-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO       TO WAML-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PRINCIPAL DO PROGRAMA                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-ABRIR-CURSOR.

           PERFORM 2200-LER-CURSOR.

           PERFORM 2300-PROCESSAR-CURSOR UNTIL
                   WRK-FIM-CURSOR        EQUAL 'S'.

           PERFORM 2400-FECHAR-CURSOR.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ABRIR CURSOR DA TABELA GFCTB0M2                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-ABRIR-CURSOR               SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO IND-1
           MOVE SPACES                 TO WRK-FIM-CURSOR.

           MOVE WAMK-FILTRO-AGENCIA    TO CAG-BCRIA        OF GFCTB0M2.
           MOVE WAMK-FILTRO-CONTA      TO CCTA-CLI         OF GFCTB0M2.

           IF  WAMK-PONTEIRO-SEQUENCIA EQUAL ZEROS
               MOVE 99999              TO NSEQ-REG-CLULR   OF GFCTB0M2
           ELSE
               MOVE WAMK-PONTEIRO-SEQUENCIA
                                       TO NSEQ-REG-CLULR   OF GFCTB0M2
           END-IF.

           IF  WAMK-PONTEIRO-TIMESTAMP
                                       EQUAL SPACES
               MOVE '3333-01-01-01.01.01.000001'
                                       TO HINCL-REG        OF GFCTB0M2
           ELSE
               MOVE WAMK-PONTEIRO-TIMESTAMP
                                       TO HINCL-REG        OF GFCTB0M2
           END-IF.

           EXEC SQL
               OPEN CSR01-GFCTB0M2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'TADSAO_INDVD_CLULR' TO WRK-NOM-TAB
               MOVE 'OPEN  '           TO WRK-COMANDO-SQL
               MOVE '0001'             TO WRK-LOCAL
               MOVE  0005              TO WAML-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0M2'         TO WRK-INC-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA FORMATACAO PADRAO DE ERROS DB2.                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-FORMATAR-ERRO-DB2          SECTION.
      *----------------------------------------------------------------*

           PERFORM 1310-OBTER-DESC-MENSAGEM.

           STRING GFCTG3-DESC-MSG ' - '
                  WRK-INC-TAB
           DELIMITED BY '  '           INTO WAML-DESC-MSG-ERRO.

           MOVE  9                     TO WAML-ERRO.
           MOVE 'GFCT0335'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE  SQLCA                 TO GFCT0M-SQLCA-AREA.
           MOVE  SQLCODE               TO GFCT0M-SQLCODE
                                          WRK-TAM-09-COM-S
           MOVE WRK-TAM-09-SEM-S(7:3)  TO WAML-COD-SQL-ERRO.
           MOVE WRK-NOM-TAB            TO GFCT0M-NOME-TAB.
           MOVE WRK-COMANDO-SQL        TO GFCT0M-COMANDO-SQL.
           MOVE WRK-LOCAL              TO GFCT0M-LOCAL.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * LER CURSOR DA TABELA GFCTB0M2                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-LER-CURSOR                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR01-GFCTB0M2 INTO
                   :GFCTB0M2.CSERVC-TARIF,
                   :GFCTB0M2.HINCL-REG,
                   :GFCTB0M2.NSEQ-REG-CLULR,
                   :GFCTB0M2.COPER-FONE-MOVEL,
                   :GFCTB0M2.CDDD-FONE-MOVEL,
022012             :GFCTB0M2.NLIN-TFONI,
                   :GFCTB0M2.CFUNC-MANUT
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'TADSAO_INDVD_CLULR' TO WRK-NOM-TAB
               MOVE 'FETCH '           TO WRK-COMANDO-SQL
               MOVE '0002'             TO WRK-LOCAL
               MOVE  0006              TO WAML-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0M2'         TO WRK-INC-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR
                                          WAML-FIM
               IF  IND-1               EQUAL ZEROS
                   PERFORM 2400-FECHAR-CURSOR
                   MOVE 1              TO WAML-ERRO
                   MOVE ZEROS          TO WAML-COD-SQL-ERRO
                   MOVE 1670           TO WAML-COD-MSG-ERRO
                   MOVE '1670'         TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG       TO WAML-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR DADOS OBTIDOS NO CURSOR                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-PROCESSAR-CURSOR           SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO IND-1.

           IF  IND-1                   GREATER 7
               MOVE 'S'                TO WRK-FIM-CURSOR

               MOVE NSEQ-REG-CLULR     OF GFCTB0M2
                                       TO WRK-TAM-05-COM-S
               MOVE WRK-TAM-05-SEM-S   TO WAML-PONTEIRO-SEQUENCIA

               MOVE HINCL-REG          OF GFCTB0M2
                                       TO WAML-PONTEIRO-TIMESTAMP
               GO TO 2300-99-FIM
           END-IF.

           ADD 105                     TO WAML-LL.

           MOVE WAML-QTDE-TOT-REG      TO WRK-QTDE-TOT-REG.
           ADD 1                       TO WRK-QTDE-TOT-REG.
           MOVE WRK-QTDE-TOT-REG       TO WAML-QTDE-TOT-REG

           PERFORM 2310-ACESSAR-GFCTB0D8

           PERFORM 2320-OBTER-DADOS-CLIENTE.

           MOVE GFCTHA-NOME-CLIENTE(1:40)
                                       TO WAML-NOME-CLIENTE

           MOVE CSERVC-TARIF           OF GFCTB0M2
                                       TO WRK-TAM-05-COM-S
           MOVE WRK-TAM-05-SEM-S       TO WAML-TARIFA-ADESAO(IND-1).

           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8(1:40)
                                       TO WAML-DESCR-TARIFA(IND-1).

           MOVE COPER-FONE-MOVEL       OF GFCTB0M2
                                       TO WRK-TAM-09-COM-S
           MOVE WRK-TAM-09-SEM-S       TO WAML-CD-OPERADORA(IND-1).

           MOVE CDDD-FONE-MOVEL        OF GFCTB0M2
                                       TO WRK-TAM-03-COM-S
           MOVE WRK-TAM-03-SEM-S       TO WAML-CD-DDD(IND-1).

022012     MOVE NLIN-TFONI             OF GFCTB0M2
022012                                 TO WRK-TAM-11-COM-S
022012     MOVE WRK-TAM-11-SEM-S       TO WAML-NR-TELEFONE(IND-1).


           PERFORM 2330-ACESSAR-GFCTB009

           IF  CINDCD-EXCL-REG         OF GFCTB009
                                       EQUAL ZEROS
               MOVE SPACES             TO WAML-SITUACAO-ADESAO(IND-1)
           ELSE
               MOVE 'CANCELADA'        TO WAML-SITUACAO-ADESAO(IND-1)
           END-IF.

           MOVE HINCL-REG              OF GFCTB0M2
                                       TO WAML-TIMESTAMP(IND-1)
           MOVE NSEQ-REG-CLULR         OF GFCTB0M2
                                       TO WRK-TAM-05-COM-S
           MOVE WRK-TAM-05-SEM-S       TO WAML-SEQUENCIA(IND-1)

           PERFORM 2200-LER-CURSOR.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER DADOS DA TARIFA                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2310-ACESSAR-GFCTB0D8           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0M2
                                       TO CSERVC-TARIF     OF GFCTB0D8.

           EXEC SQL
             SELECT
                   RSERVC_TARIF_REDZD
             INTO
                   :GFCTB0D8.RSERVC-TARIF-REDZD
             FROM   DB2PRD.SERVC_TARIF_PRINC
             WHERE
                   CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SERVC_TARIF_PRINC' TO WRK-NOM-TAB
               MOVE 'SELECT'           TO WRK-COMANDO-SQL
               MOVE '0003'             TO WRK-LOCAL
               MOVE  0012              TO WAML-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0D8'         TO WRK-INC-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA OBTER O NOME DO CLIENTE                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2320-OBTER-DADOS-CLIENTE        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG0-ENTRADA.
           MOVE 100                    TO GFCTG0-LL.
           MOVE ZEROS                  TO GFCTG0-ZZ.
           MOVE WAMK-TRANSACAO         TO GFCTG0-TRANSACAO.
           MOVE WAMK-FUNCAO            TO GFCTG0-FUNCAO.
           MOVE WAMK-FUNC-BDSCO        TO GFCTG0-FUNC-BDSCO.
           MOVE WAMK-FILTRO-AGENCIA    TO GFCTG0-COD-DEPDC.
           MOVE WAMK-FILTRO-CONTA      TO GFCTG0-CCTA-CLI.
           MOVE 'N'                    TO GFCTG0-FIM.

           MOVE SPACES                 TO GFCTHA-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE  GFCTHA-SAIDA
                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5054'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTG0-ENTRADA
                                             GFCTHA-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO WAML-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0131'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

BI0216     IF  GFCTHA-COD-MSG-ERRO  EQUAL  1428
BI0216         GO     TO  2320-99-FIM
BI0216     END-IF.

           IF  GFCTHA-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO WAML-FIM
               MOVE GFCTHA-ERRO        TO WAML-ERRO
               MOVE GFCTHA-COD-SQL-ERRO
                                       TO WAML-COD-SQL-ERRO
               MOVE GFCTHA-COD-MSG-ERRO
                                       TO WAML-COD-MSG-ERRO
               IF  GFCTHA-ERRO         EQUAL 1
                   MOVE GFCTHA-DESC-MSG-ERRO
                                       TO WAML-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5054-' GFCTHA-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO WAML-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER INFORMACAO DA SITUACAO                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2330-ACESSAR-GFCTB009           SECTION.
      *----------------------------------------------------------------*

           MOVE CAG-BCRIA              OF GFCTB0M2
                                       TO CJUNC-DEPDC      OF GFCTB009.
           MOVE CCTA-CLI               OF GFCTB0M2
                                       TO CCTA-CLI         OF GFCTB009.
           MOVE CSERVC-TARIF           OF GFCTB0M2
                                       TO CSERVC-TARIF     OF GFCTB009.
           MOVE HINCL-REG              OF GFCTB0M2
                                       TO HINCL-REG        OF GFCTB009.

           EXEC SQL
             SELECT
                    CINDCD_EXCL_REG
             INTO
                   :GFCTB009.CINDCD-EXCL-REG
             FROM   DB2PRD.ADSAO_INDVD_PCOTE
             WHERE
                   CJUNC_DEPDC        = :GFCTB009.CJUNC-DEPDC
               AND CCTA_CLI           = :GFCTB009.CCTA-CLI
               AND CSERVC_TARIF       = :GFCTB009.CSERVC-TARIF
               AND HINCL_REG          = :GFCTB009.HINCL-REG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'ADSAO_INDVD_PCOTE' TO WRK-NOM-TAB
               MOVE 'SELECT'           TO WRK-COMANDO-SQL
               MOVE '0004'             TO WRK-LOCAL
               MOVE  0012              TO WAML-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0D8'         TO WRK-INC-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       2330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * FECHAR CURSOR DA TABELA GFCTB0M2                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-FECHAR-CURSOR              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-GFCTB0M2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'TADSAO_INDVD_CLULR' TO WRK-NOM-TAB
               MOVE 'CLOSE '           TO WRK-COMANDO-SQL
               MOVE '0005'             TO WRK-LOCAL
               MOVE  0011              TO WAML-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0M2'         TO WRK-INC-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA FINALIZAR PROCESSAMENTO                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
