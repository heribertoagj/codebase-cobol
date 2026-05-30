      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0304.
       AUTHOR.     LEANDRO MENDES.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0304                                    *
      *    PROGRAMADOR.:   LEANDRO MENDES          - CPM PATO BRANCO   *
      *    ANALISTA CPM:   EDSON DOS SANTOS        - CPM PATO BRANCO   *
      *    ANALISTA....:   MARCELO                 - PROCKWORK GP/50   *
      *    DATA........:   02/05/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CANCELAMENTO DE SUSPENSAO DE TARIFA         *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                         INCLUDE/BOOK    *
      *                    DB2PRD.PARM_DATA_PROCM        GFCTB0A1      *
      *                    DB2PRD.SUSP_TARIF_AGPTO       GFCTB0E7      *
      *                    DB2PRD.SUSP_GRP_CLI           GFCTB0E3      *
      *                    DB2PRD.SUSP_GRP_DEPDC         GFCTB0E4      *
      *                    DB2PRD.SUSP_GRP_MUN           GFCTB0E5      *
      *                    DB2PRD.SUSP_GRP_UF            GFCTB0E6      *
      *                    DB2PRD.TSUSP_GRP_PAB          GFCTB0I1      *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#GFCTP8 - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCTP9 - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCTG2 - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5523 *
      *    I#GFCTG3 - AREA DE COMUNICACAO - SAIDA   DO MODULO GFCT5523 *
      *    I#GFCTFZ - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5522 *
      *    I#GFCTG1 - AREA DE COMUNICACAO - SAIDA   DO MODULO GFCT5522 *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS                      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL0081 - ALOCA DINAMICAMENTE UM MODULO                    *
      *    POOL7600 - OBTER DATA E HORA                                *
      *    GFCT5522 - VERIFICA ON-LINE                                 *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS                     *
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
           '* AREAS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-INIC-ANT                PIC  9(008)         VALUE ZEROS.
       77  WRK-FIM-NOVA                PIC  9(008)         VALUE ZEROS.
       77  WRK-FIM-ANT                 PIC  9(008)         VALUE ZEROS.
       77  WRK-DPROCM-ATUAL            PIC  9(008)         VALUE ZEROS.
       77  WRK-NOM-TAB                 PIC  X(008)         VALUE SPACES.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-ERRO-DATA               PIC  X(001)         VALUE SPACES.

       01  WRK-DATA-DB2                PIC  X(010)         VALUE SPACES.
       01  WRK-DATA-DB2-R              REDEFINES WRK-DATA-DB2.
           05  WRK-DIA-DB2             PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-MES-DB2             PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-ANO-DB2             PIC  9(004).

       01  WRK-DATA-POOL1470           PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-POOL1470-R         REDEFINES WRK-DATA-POOL1470.
           05  WRK-DIA-POOL1470        PIC  9(002).
           05  WRK-MES-POOL1470        PIC  9(002).
           05  WRK-ANO-POOL1470        PIC  9(004).

       01  WRK-DATA-INV                PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-INV-R              REDEFINES WRK-DATA-INV.
           05  WRK-ANO-INV             PIC  9(004).
           05  WRK-MES-INV             PIC  9(002).
           05  WRK-DIA-INV             PIC  9(002).

       01  WRK-FUNC-BDSCO.
           03  WRK-FUNC-BDSCO-N        PIC  9(007)         VALUE ZEROS.

       01  WRK-MSG01.
           05  FILLER                  PIC  X(026)         VALUE
               'ERRO NA CHAMADA DO MODULO'.
           05  WRK-NOME-MOD            PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREAS DE COMUNICACAO COM GFCT5522 *'.
      *----------------------------------------------------------------*

       COPY I#GFCTFZ.

       COPY I#GFCTG1.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREAS DE COMUNICACAO COM GFCT5523 *'.
      *----------------------------------------------------------------*

       COPY I#GFCTG2.

       COPY I#GFCTG3.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0E7
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0E3
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0E4
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0E5
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0E6
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0I1
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY I#GFCTP8.

       COPY I#GFCTP9.

       COPY I#GFCT0M.

      *================================================================*
       PROCEDURE DIVISION              USING GFCTP8-ENTRADA
                                             GFCTP9-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *ROTINA INICIAL DO PROGRAMA                                      *
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
      *ROTINA PARA INICIALIZAR DADOS                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          GFCTP9-SAIDA.

           INITIALIZE GFCT0M-AREA-ERROS
                      GFCTP9-SAIDA.

           PERFORM 1100-VERIFICAR-SIST-DISPONIVEL.

           PERFORM 1200-INICIALIZAR-SAIDA.

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

           MOVE 'GFCT5522'                     TO WRK-MODULO.
           MOVE 'GFCT'                         TO GFCTFZ-SISTEMA.
           MOVE GFCTP8-TRANSACAO               TO GFCTFZ-TRANSACAO.
           MOVE GFCTP8-FUNCAO                  TO GFCTFZ-FUNCAO.
           MOVE GFCTP8-FUNC-BDSCO              TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                            TO GFCTFZ-FIM.

           CALL 'POOL0081'                     USING WRK-MODULO
                                                     GFCTFZ-ENTRADA
                                                     GFCTG1-SAIDA
                                                     GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                     NOT EQUAL ZEROS
               MOVE 9                          TO GFCTP9-ERRO
               MOVE WRK-MODULO                 TO WRK-NOME-MOD
                                                  GFCT0M-PROGRAMA
               MOVE WRK-MSG01                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0304'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO                     EQUAL 9
               MOVE 'GFCT0304'                 TO GFCT0M-TRANSACAO
               MOVE GFCTG1-COD-SQL-ERRO        TO GFCTP9-COD-SQL-ERRO
                                                  GFCT0M-SQLCODE
               MOVE GFCTG1-ERRO                TO GFCTP9-ERRO
               MOVE GFCTG1-COD-MSG-ERRO        TO GFCTP9-COD-MSG-ERRO
               MOVE GFCTG1-DESC-MSG-ERRO       TO GFCT0M-TEXTO
                                                  GFCTP9-DESC-MSG-ERRO
      *        MOVE WRK-MODULO                 TO GFCT0M-PROGRAMA
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE              EQUAL 1
               MOVE 9                          TO GFCTP9-ERRO
               MOVE 'SISTEMA INDISPONIVEL'     TO GFCT0M-TEXTO
               MOVE WRK-MODULO                 TO GFCT0M-PROGRAMA
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0304'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA INICIALIZAR SAIDA                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-INICIALIZAR-SAIDA          SECTION.
      *----------------------------------------------------------------*

           MOVE 1010                   TO GFCTP9-LL.
           MOVE ZEROS                  TO GFCTP9-ZZ.
           MOVE GFCTP8-TRANSACAO       TO GFCTP9-TRANSACAO.
           MOVE GFCTP8-FUNCAO          TO GFCTP9-FUNCAO.
           MOVE GFCTP8-QTDE-OCOR       TO GFCTP9-QTDE-OCOR.
           MOVE GFCTP8-QTDE-TOT-REG    TO GFCTP9-QTDE-TOT-REG.
           MOVE GFCTP8-FUNC-BDSCO      TO GFCTP9-FUNC-BDSCO.
           MOVE GFCTP8-AGPTO           TO GFCTP9-AGPTO
                                       OF GFCTP9-DADOS-RST.
           MOVE GFCTP8-ACAO            TO GFCTP9-ACAO.
           MOVE 'S'                    TO GFCTP9-FIM.

           MOVE GFCTP8-TARIFA          TO GFCTP9-TARIFA.
           MOVE GFCTP8-AGPTO           TO GFCTP9-AGPTO
                                       OF GFCTP9-FIXO.
           MOVE GFCTP8-SEQUENCIA       TO GFCTP9-SEQUENCIA.
           MOVE GFCTP8-DATA-INICIO-ANT TO GFCTP9-DATA-INICIO-ANT.
           MOVE GFCTP8-DATA-FIM-ANT    TO GFCTP9-DATA-FIM-ANT.
           MOVE GFCTP8-DATA-FIM-NOVA   TO GFCTP9-DATA-FIM.

           MOVE ZEROS                  TO GFCTP9-COD-SQL-ERRO
                                          GFCTP9-COD-MSG-ERRO
                                          GFCTP9-ERRO.
           MOVE '0000'                 TO GFCTG2-COD-MSG.

           PERFORM 1210-OBTER-DESC-MENSAGEM.

           MOVE GFCTG3-DESC-MSG        TO GFCTP9-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA OBTER DESCRICAO MENSAGEM                            *
      ******************************************************************
      *----------------------------------------------------------------*
       1210-OBTER-DESC-MENSAGEM        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                   TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE 100                      TO GFCTG2-LL.
           MOVE ZEROS                    TO GFCTG2-ZZ.
           MOVE GFCTP8-TRANSACAO         TO GFCTG2-TRANSACAO.
           MOVE GFCTP8-FUNCAO            TO GFCTG2-FUNCAO.
           MOVE GFCTP8-FUNC-BDSCO        TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                      TO GFCTG2-TIPO-PROC.
           MOVE 'N'                      TO GFCTG2-FIM.

           MOVE 'GFCT5523'               TO WRK-MODULO.

           CALL 'POOL0081'               USING WRK-MODULO
                                               GFCTG2-ENTRADA
                                               GFCTG3-SAIDA
                                               GFCT0M-AREA-ERROS.

           IF  RETURN-CODE               NOT EQUAL ZEROS OR
               GFCTG3-ERRO               EQUAL 9
               MOVE 9                    TO GFCTP9-ERRO
               MOVE WRK-MODULO           TO GFCT0M-PROGRAMA
                                            WRK-NOME-MOD
               MOVE WRK-MSG01            TO GFCT0M-TEXTO
               MOVE 'APL'                TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0304'           TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO               EQUAL 1
               MOVE 1                    TO GFCTP9-ERRO
               MOVE GFCTG3-COD-SQL-ERRO  TO GFCTP9-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO  TO GFCTP9-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO TO GFCTP9-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR DADOS                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF  GFCTP8-TRANSACAO            EQUAL LOW-VALUES  OR
               GFCTP8-TRANSACAO            EQUAL SPACES      OR
               GFCTP8-FUNCAO               EQUAL LOW-VALUES  OR
               GFCTP8-FUNCAO               EQUAL SPACES      OR
               GFCTP8-FUNC-BDSCO           EQUAL LOW-VALUES  OR
               GFCTP8-FUNC-BDSCO           EQUAL SPACES      OR
               GFCTP8-TARIFA               NOT NUMERIC       OR
               GFCTP8-TARIFA               EQUAL ZEROS       OR
               GFCTP8-AGPTO                NOT NUMERIC       OR
               GFCTP8-SEQUENCIA            NOT NUMERIC       OR
               GFCTP8-SEQUENCIA            EQUAL ZEROS       OR
              (GFCTP8-ACAO                 NOT EQUAL '1' AND '2') OR
              (GFCTP8-AGPTO                NOT EQUAL 4 AND 14 AND 15 AND
                                                     18 AND 19)
               MOVE 1                      TO GFCTP9-ERRO
                                              GFCTP9-COD-MSG-ERRO
               MOVE ZEROS                  TO GFCTP9-COD-SQL-ERRO
               MOVE '0001'                 TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG        TO GFCTP9-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTP8-ACAO                 EQUAL '2'
               IF  GFCTP8-DATA-INICIO-ANT  EQUAL SPACES OR
                   GFCTP8-DATA-INICIO-ANT  EQUAL LOW-VALUES OR
                   GFCTP8-DATA-FIM-ANT     EQUAL SPACES OR
                   GFCTP8-DATA-FIM-ANT     EQUAL LOW-VALUES OR
                   GFCTP8-DATA-FIM-NOVA    EQUAL SPACES OR
                   GFCTP8-DATA-FIM-NOVA    EQUAL LOW-VALUES
                   MOVE 897                TO GFCTP9-ERRO
                                              GFCTP9-COD-MSG-ERRO
                   MOVE ZEROS              TO GFCTP9-COD-SQL-ERRO
                   MOVE '0897'             TO GFCTG2-COD-MSG
                   PERFORM 1210-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG    TO GFCTP9-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           ELSE
               IF GFCTP8-DATA-INICIO-ANT EQUAL SPACES
                   MOVE 980                TO GFCTP9-ERRO
                                              GFCTP9-COD-MSG-ERRO
                   MOVE ZEROS              TO GFCTP9-COD-SQL-ERRO
                   MOVE '0980'             TO GFCTG2-COD-MSG
                   PERFORM 1210-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG    TO GFCTP9-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 1310-SELECIONAR-GFCTB0A1.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *SELECIONAR TABELA GFCTB0A1                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1310-SELECIONAR-GFCTB0A1        SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ATUAL
             INTO
                   :GFCTB0A1.DPROCM-ATUAL
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC        = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE  SPACES            TO GFCT0M-AREA-ERROS
               INITIALIZE GFCT0M-AREA-ERROS
               MOVE 'PARM_DATA_PROCM ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTP9-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A1'         TO WRK-NOM-TAB
               PERFORM 1311-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FORMATA ERROS DB2                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       1311-FORMATAR-ERRO-DB2          SECTION.
      *----------------------------------------------------------------*

           MOVE  9                     TO GFCTP9-ERRO.
           MOVE 'GFCT0304'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE  SQLCA                 TO GFCT0M-SQLCA-AREA.
           MOVE  SQLCODE               TO GFCT0M-SQLCODE
                                          GFCTP9-COD-SQL-ERRO.

           PERFORM 1210-OBTER-DESC-MENSAGEM.

           STRING GFCTG3-DESC-MSG ' - ' WRK-NOM-TAB
           DELIMITED BY '  '           INTO GFCTP9-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1311-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PRINCIPAL DO PROGRAMA                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  GFCTP8-ACAO             EQUAL '2'
               PERFORM 2100-CONSISTIR-DATAS
               PERFORM 2200-ENCERRAR-SUSPENSAO
           ELSE
               PERFORM 2300-CONSULTAR-SUSPENSAO
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTIR DATAS                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-CONSISTIR-DATAS            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTP8-DATA-INICIO-ANT TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-INV.
           MOVE WRK-MES-DB2            TO WRK-MES-INV.
           MOVE WRK-ANO-DB2            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-INIC-ANT

           MOVE GFCTP8-DATA-FIM-ANT    TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-INV.
           MOVE WRK-MES-DB2            TO WRK-MES-INV.
           MOVE WRK-ANO-DB2            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-FIM-ANT.

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-INV.
           MOVE WRK-MES-DB2            TO WRK-MES-INV.
           MOVE WRK-ANO-DB2            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-DPROCM-ATUAL.

           MOVE GFCTP8-DATA-FIM-NOVA   TO WRK-DATA-DB2.
           MOVE 'N'                    TO WRK-ERRO-DATA.

           IF  WRK-DATA-DB2            EQUAL SPACES OR LOW-VALUES
               MOVE 'S'                TO WRK-ERRO-DATA
           ELSE
               MOVE WRK-DIA-DB2        TO WRK-DIA-POOL1470
                                          WRK-DIA-INV
               MOVE WRK-MES-DB2        TO WRK-MES-POOL1470
                                          WRK-MES-INV
               MOVE WRK-ANO-DB2        TO WRK-ANO-POOL1470
                                          WRK-ANO-INV
               MOVE WRK-DATA-INV       TO WRK-FIM-NOVA
               PERFORM 2110-ACESSAR-POOL1470
           END-IF.

           IF  WRK-ERRO-DATA           EQUAL 'S'
               MOVE 1                  TO GFCTP9-ERRO
               MOVE ZEROS              TO GFCTP9-COD-SQL-ERRO
               MOVE 893                TO GFCTP9-COD-MSG-ERRO
               MOVE '0893'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTP9-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-FIM-NOVA            NOT LESS WRK-FIM-ANT
               MOVE 1                  TO GFCTP9-ERRO
               MOVE ZEROS              TO GFCTP9-COD-SQL-ERRO
               MOVE 894                TO GFCTP9-COD-MSG-ERRO
               MOVE '0894'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTP9-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-FIM-NOVA            LESS WRK-INIC-ANT
               MOVE 1                  TO GFCTP9-ERRO
               MOVE ZEROS              TO GFCTP9-COD-SQL-ERRO
               MOVE 895                TO GFCTP9-COD-MSG-ERRO
               MOVE '0895'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTP9-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-FIM-NOVA            LESS WRK-DPROCM-ATUAL
               MOVE 1                  TO GFCTP9-ERRO
               MOVE ZEROS              TO GFCTP9-COD-SQL-ERRO
               MOVE 896                TO GFCTP9-COD-MSG-ERRO
               MOVE '0896'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTP9-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTE DATA ATRAVES DA POOL1470                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-ACESSAR-POOL1470           SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1470'             USING WRK-DATA-POOL1470.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'S'                TO WRK-ERRO-DATA
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ENCERRA SUSPENSAO                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-ENCERRAR-SUSPENSAO         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INIC-ANT            GREATER WRK-DPROCM-ATUAL AND
               WRK-INIC-ANT            EQUAL   WRK-FIM-NOVA
               PERFORM 2210-EXCLUIR-FISICAMENTE
           ELSE
               PERFORM 2220-EXCLUIR-LOGICAMENTE
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EFETUA EXCLUSAO FISICA                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-EXCLUIR-FISICAMENTE        SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTP8-AGPTO
               WHEN 4
                   PERFORM 2211-EXCLUIR-CLIENTE
               WHEN 14
                   PERFORM 2212-EXCLUIR-AGENCIA
               WHEN 15
                   PERFORM 2213-EXCLUIR-POSTO
               WHEN 18
                   PERFORM 2214-EXCLUIR-MUNICIPIO
               WHEN 19
                   PERFORM 2215-EXCLUIR-UF
           END-EVALUATE.

           PERFORM 2216-EXCLUIR-PRINCIPAL.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EFETUA DELETE DA TABELA GFCTB0E3                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2211-EXCLUIR-CLIENTE            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTP8-TARIFA          TO CSERVC-TARIF     OF GFCTB0E3.
           MOVE GFCTP8-AGPTO           TO CAGPTO-CTA       OF GFCTB0E3.
           MOVE GFCTP8-SEQUENCIA       TO CSEQ-AGPTO-CTA   OF GFCTB0E3.
           MOVE GFCTP8-DATA-INICIO-ANT TO DINIC-SUSP-COBR  OF GFCTB0E3.

           EXEC SQL
             DELETE FROM    DB2PRD.SUSP_GRP_CLI
             WHERE
                   CSERVC_TARIF       = :GFCTB0E3.CSERVC-TARIF       AND
                   DINIC_SUSP_COBR    = :GFCTB0E3.DINIC-SUSP-COBR    AND
                   CAGPTO_CTA         = :GFCTB0E3.CAGPTO-CTA         AND
                   CSEQ_AGPTO_CTA     = :GFCTB0E3.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE  SPACES            TO GFCT0M-AREA-ERROS
               INITIALIZE GFCT0M-AREA-ERROS
               MOVE 'SUSP_GRP_CLI    ' TO GFCT0M-NOME-TAB
               MOVE 'DELETE  '         TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE  0020              TO GFCTP9-COD-MSG-ERRO
               MOVE '0020'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0E3'         TO WRK-NOM-TAB
               PERFORM 1311-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTP9-ERRO
               MOVE ZEROS              TO GFCTP9-COD-SQL-ERRO
               MOVE 900                TO GFCTP9-COD-MSG-ERRO
               MOVE '0900'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTP9-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2211-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EFETUA DELETE DA TABELA GFCTB0E4                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2212-EXCLUIR-AGENCIA            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTP8-TARIFA          TO CSERVC-TARIF     OF GFCTB0E4.
           MOVE GFCTP8-AGPTO           TO CAGPTO-CTA       OF GFCTB0E4.
           MOVE GFCTP8-SEQUENCIA       TO CSEQ-AGPTO-CTA   OF GFCTB0E4.
           MOVE GFCTP8-DATA-INICIO-ANT TO DINIC-SUSP-COBR  OF GFCTB0E4.

           EXEC SQL
             DELETE FROM    DB2PRD.SUSP_GRP_DEPDC
             WHERE
                   CSERVC_TARIF       = :GFCTB0E4.CSERVC-TARIF       AND
                   DINIC_SUSP_COBR    = :GFCTB0E4.DINIC-SUSP-COBR    AND
                   CAGPTO_CTA         = :GFCTB0E4.CAGPTO-CTA         AND
                   CSEQ_AGPTO_CTA     = :GFCTB0E4.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE  SPACES            TO GFCT0M-AREA-ERROS
               INITIALIZE GFCT0M-AREA-ERROS
               MOVE 'SUSP_GRP_DEPDC  ' TO GFCT0M-NOME-TAB
               MOVE 'DELETE  '         TO GFCT0M-COMANDO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE  0020              TO GFCTP9-COD-MSG-ERRO
               MOVE '0020'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0E4'         TO WRK-NOM-TAB
               PERFORM 1311-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTP9-ERRO
               MOVE ZEROS              TO GFCTP9-COD-SQL-ERRO
               MOVE 900                TO GFCTP9-COD-MSG-ERRO
               MOVE '0900'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTP9-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2212-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EFETUA DELETE DA TABELA GFCTB0I1                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2213-EXCLUIR-POSTO              SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTP8-TARIFA          TO CSERVC-TARIF     OF GFCTB0I1.
           MOVE GFCTP8-AGPTO           TO CAGPTO-CTA       OF GFCTB0I1.
           MOVE GFCTP8-SEQUENCIA       TO CSEQ-AGPTO-CTA   OF GFCTB0I1.
           MOVE GFCTP8-DATA-INICIO-ANT TO DINIC-SUSP-COBR  OF GFCTB0I1.

           EXEC SQL
             DELETE FROM    DB2PRD.TSUSP_GRP_PAB
             WHERE
                   CSERVC_TARIF       = :GFCTB0I1.CSERVC-TARIF       AND
                   DINIC_SUSP_COBR    = :GFCTB0I1.DINIC-SUSP-COBR    AND
                   CAGPTO_CTA         = :GFCTB0I1.CAGPTO-CTA         AND
                   CSEQ_AGPTO_CTA     = :GFCTB0I1.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE  SPACES            TO GFCT0M-AREA-ERROS
               INITIALIZE GFCT0M-AREA-ERROS
               MOVE 'TSUSP_GRP_PAB   ' TO GFCT0M-NOME-TAB
               MOVE 'DELETE  '         TO GFCT0M-COMANDO-SQL
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE  0020              TO GFCTP9-COD-MSG-ERRO
               MOVE '0020'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0I1'         TO WRK-NOM-TAB
               PERFORM 1311-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTP9-ERRO
               MOVE ZEROS              TO GFCTP9-COD-SQL-ERRO
               MOVE 900                TO GFCTP9-COD-MSG-ERRO
               MOVE '0900'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTP9-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2213-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EFETUA DELETE DA TABELA GFCTB0E5                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2214-EXCLUIR-MUNICIPIO          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTP8-TARIFA          TO CSERVC-TARIF     OF GFCTB0E5.
           MOVE GFCTP8-AGPTO           TO CAGPTO-CTA       OF GFCTB0E5.
           MOVE GFCTP8-SEQUENCIA       TO CSEQ-AGPTO-CTA   OF GFCTB0E5.
           MOVE GFCTP8-DATA-INICIO-ANT TO DINIC-SUSP-COBR  OF GFCTB0E5.

           EXEC SQL
             DELETE FROM    DB2PRD.SUSP_GRP_MUN
             WHERE
                   CSERVC_TARIF       = :GFCTB0E5.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB0E5.CAGPTO-CTA         AND
                   DINIC_SUSP_COBR    = :GFCTB0E5.DINIC-SUSP-COBR    AND
                   CSEQ_AGPTO_CTA     = :GFCTB0E5.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND + 100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE  SPACES            TO GFCT0M-AREA-ERROS
               INITIALIZE GFCT0M-AREA-ERROS
               MOVE 'SUSP_GRP_MUN    ' TO GFCT0M-NOME-TAB
               MOVE 'DELETE  '         TO GFCT0M-COMANDO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE  0020              TO GFCTP9-COD-MSG-ERRO
               MOVE '0020'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0E5'         TO WRK-NOM-TAB
               PERFORM 1311-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTP9-ERRO
               MOVE ZEROS              TO GFCTP9-COD-SQL-ERRO
               MOVE 900                TO GFCTP9-COD-MSG-ERRO
               MOVE '0900'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTP9-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2214-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EFETUA DELETE DA TABELA GFCTB0E6                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2215-EXCLUIR-UF                 SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTP8-TARIFA          TO CSERVC-TARIF     OF GFCTB0E6.
           MOVE GFCTP8-AGPTO           TO CAGPTO-CTA       OF GFCTB0E6.
           MOVE GFCTP8-SEQUENCIA       TO CSEQ-AGPTO-CTA   OF GFCTB0E6.
           MOVE GFCTP8-DATA-INICIO-ANT TO DINIC-SUSP-COBR  OF GFCTB0E6.

           EXEC SQL
             DELETE FROM    DB2PRD.SUSP_GRP_UF
             WHERE
                   CSERVC_TARIF       = :GFCTB0E6.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB0E6.CAGPTO-CTA         AND
                   DINIC_SUSP_COBR    = :GFCTB0E6.DINIC-SUSP-COBR    AND
                   CSEQ_AGPTO_CTA     = :GFCTB0E6.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE  SPACES            TO GFCT0M-AREA-ERROS
               INITIALIZE GFCT0M-AREA-ERROS
               MOVE 'SUSP_GRP_UF     ' TO GFCT0M-NOME-TAB
               MOVE 'DELETE  '         TO GFCT0M-COMANDO-SQL
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE  0020              TO GFCTP9-COD-MSG-ERRO
               MOVE '0020'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0E6'         TO WRK-NOM-TAB
               PERFORM 1311-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTP9-ERRO
               MOVE ZEROS              TO GFCTP9-COD-SQL-ERRO
               MOVE 900                TO GFCTP9-COD-MSG-ERRO
               MOVE '0900'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTP9-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2215-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EFETUA DELETE DA TABELA GFCTB0E7                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2216-EXCLUIR-PRINCIPAL          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTP8-TARIFA          TO CSERVC-TARIF     OF GFCTB0E7.
           MOVE GFCTP8-AGPTO           TO CAGPTO-CTA       OF GFCTB0E7.
           MOVE GFCTP8-SEQUENCIA       TO CSEQ-AGPTO-CTA   OF GFCTB0E7.
           MOVE GFCTP8-DATA-INICIO-ANT TO DINIC-SUSP-COBR  OF GFCTB0E7.

           EXEC SQL
             DELETE FROM    DB2PRD.SUSP_TARIF_AGPTO
             WHERE
                   CSERVC_TARIF       = :GFCTB0E7.CSERVC-TARIF       AND
                   DINIC_SUSP_COBR    = :GFCTB0E7.DINIC-SUSP-COBR    AND
                   CAGPTO_CTA         = :GFCTB0E7.CAGPTO-CTA         AND
                   CSEQ_AGPTO_CTA     = :GFCTB0E7.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE  SPACES            TO GFCT0M-AREA-ERROS
               INITIALIZE GFCT0M-AREA-ERROS
               MOVE 'SUSP_TARIF_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'DELETE  '         TO GFCT0M-COMANDO-SQL
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE  0020              TO GFCTP9-COD-MSG-ERRO
               MOVE '0020'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0E7'         TO WRK-NOM-TAB
               PERFORM 1311-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTP9-ERRO
               MOVE ZEROS              TO GFCTP9-COD-SQL-ERRO
               MOVE 900                TO GFCTP9-COD-MSG-ERRO
               MOVE '0900'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTP9-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2216-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EFETUA EXCLUSAO LOGICA                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2220-EXCLUIR-LOGICAMENTE        SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTP8-TARIFA          TO CSERVC-TARIF     OF GFCTB0E7.
           MOVE GFCTP8-AGPTO           TO CAGPTO-CTA       OF GFCTB0E7.
           MOVE GFCTP8-SEQUENCIA       TO CSEQ-AGPTO-CTA   OF GFCTB0E7.
           MOVE GFCTP8-DATA-INICIO-ANT TO DINIC-SUSP-COBR  OF GFCTB0E7.
           MOVE GFCTP8-FUNC-BDSCO      TO WRK-FUNC-BDSCO.
           MOVE WRK-FUNC-BDSCO-N       TO CFUNC-MANUT      OF GFCTB0E7.
           MOVE GFCTP8-DATA-FIM-NOVA   TO DFIM-SUSP-COBR   OF GFCTB0E7.

           EXEC SQL
             UPDATE   DB2PRD.SUSP_TARIF_AGPTO
               SET
                   CFUNC_MANUT        = :GFCTB0E7.CFUNC-MANUT,
                   HMANUT_REG         =  CURRENT TIMESTAMP,
                   DFIM_SUSP_COBR     = :GFCTB0E7.DFIM-SUSP-COBR
             WHERE
                   CSERVC_TARIF       = :GFCTB0E7.CSERVC-TARIF       AND
                   DINIC_SUSP_COBR    = :GFCTB0E7.DINIC-SUSP-COBR    AND
                   CAGPTO_CTA         = :GFCTB0E7.CAGPTO-CTA         AND
                   CSEQ_AGPTO_CTA     = :GFCTB0E7.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE  SPACES            TO GFCT0M-AREA-ERROS
               INITIALIZE GFCT0M-AREA-ERROS
               MOVE 'SUSP_TARIF_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'UPDATE  '         TO GFCT0M-COMANDO-SQL
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE  0003              TO GFCTP9-COD-MSG-ERRO
               MOVE '0003'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0E7'         TO WRK-NOM-TAB
               PERFORM 1311-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTP9-ERRO
               MOVE ZEROS              TO GFCTP9-COD-SQL-ERRO
               MOVE 900                TO GFCTP9-COD-MSG-ERRO
               MOVE '0900'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTP9-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EFETUA CONSULTA DE SUSPENCAO                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-CONSULTAR-SUSPENSAO        SECTION.
      *----------------------------------------------------------------*


           PERFORM 2360-CONSULTAR-PRINCIPAL.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EFETUA CONSULTA NA TABELA GFCTB0E7                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2360-CONSULTAR-PRINCIPAL        SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTP8-TARIFA          TO CSERVC-TARIF     OF GFCTB0E7.
           MOVE GFCTP8-AGPTO           TO CAGPTO-CTA       OF GFCTB0E7.
           MOVE GFCTP8-SEQUENCIA       TO CSEQ-AGPTO-CTA   OF GFCTB0E7.
           MOVE GFCTP8-DATA-INICIO-ANT TO DINIC-SUSP-COBR  OF GFCTB0E7.

           EXEC SQL
             SELECT
                   DINIC_SUSP_COBR,
                   DFIM_SUSP_COBR,
                   RJUSTF_SUSP_COBR
             INTO
                   :GFCTB0E7.DINIC-SUSP-COBR,
                   :GFCTB0E7.DFIM-SUSP-COBR,
                   :GFCTB0E7.RJUSTF-SUSP-COBR
             FROM   DB2PRD.SUSP_TARIF_AGPTO
             WHERE
                   CSERVC_TARIF       = :GFCTB0E7.CSERVC-TARIF       AND
                   CSEQ_AGPTO_CTA     = :GFCTB0E7.CSEQ-AGPTO-CTA     AND
                   DINIC_SUSP_COBR    = :GFCTB0E7.DINIC-SUSP-COBR    AND
                   CAGPTO_CTA         = :GFCTB0E7.CAGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE  SPACES            TO GFCT0M-AREA-ERROS
               INITIALIZE GFCT0M-AREA-ERROS
               MOVE 'SUSP_TARIF_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0140'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTP9-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0E7'         TO WRK-NOM-TAB
               PERFORM 1311-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTP9-ERRO
               MOVE ZEROS              TO GFCTP9-COD-SQL-ERRO
               MOVE 900                TO GFCTP9-COD-MSG-ERRO
               MOVE '0900'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTP9-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE DINIC-SUSP-COBR        OF GFCTB0E7
                                       TO GFCTP9-DATA-INICIO-ANT.
           MOVE DFIM-SUSP-COBR         OF GFCTB0E7
                                       TO GFCTP9-DATA-FIM-ANT.
           MOVE RJUSTF-SUSP-COBR-TEXT  OF GFCTB0E7
                                       TO GFCTP9-JUST.

      *----------------------------------------------------------------*
       2360-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA FINALIZAR PROGRAMA                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
