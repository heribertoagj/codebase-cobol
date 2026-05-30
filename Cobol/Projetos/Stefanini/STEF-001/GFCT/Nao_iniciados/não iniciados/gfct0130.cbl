      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0130.
       AUTHOR.     SIMONI FAVRETTO.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0130                                    *
      *    PROGRAMADORA:   SIMONI FAVRETTO         - CPM PATO BRANCO   *
      *    ANALISTA CPM:   ADRIANO LOPES SANT'ANA  - CPM PATO BRANCO   *
      *    ANALISTA....:   FRANCISCO               - PROCKWORK GP/50   *
      *    DATA........:   09/04/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CONSULTA DE ADESAO INDIVIDUAL A PACOTE.     *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                         INCLUDE/BOOK    *
      *                    DB2PRD.ADSAO_INDVD_PCOTE      GFCTB009      *
      *                    DB2PRD.SERVC_TARIF_PRINC      GFCTB0D8      *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#GFCTL9 - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCTLA - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCTG2 - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5523 *
      *    I#GFCTG3 - AREA DE COMUNICACAO - SAIDA DO MODULO GFCT5523   *
      *    I#GFCTFZ - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5522 *
      *    I#GFCTG1 - AREA DE COMUNICACAO - SAIDA DO MODULO GFCT5522   *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS                      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL0081 - ALOCA DINAMICAMENTE UM MODULO                    *
      *    GFCT5522 - VERIFICA ON-LINE.                                *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    GFCT5054 - OBTER DADOS DO CLIENTE.                          *

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
       77  WRK-DESPREZA                PIC  9(003)         VALUE ZEROS.
       77  WRK-NOM-TAB                 PIC  X(018)         VALUE SPACES.
       77  WRK-INC-TAB                 PIC  X(008)         VALUE SPACES.
       77  WRK-COMANDO-SQL             PIC  X(010)         VALUE SPACES.
       77  WRK-LOCAL                   PIC  X(004)         VALUE SPACES.
       77  WRK-CSERVC-TARIF            PIC  9(005)         VALUE ZEROS.

       01  WRK-MSG01.
           05  FILLER                  PIC  X(026)         VALUE
               'ERRO NA CHAMADA DO MODULO'.
           05  WRK-NOME-MOD            PIC  X(008)         VALUE SPACES.

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
               INCLUDE GFCTB0D8
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE DECLARACAO DE CURSOR *'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR01-GFCTB009 CURSOR FOR
               SELECT
                   CSERVC_TARIF,
                   DINIC_ADSAO_INDVD,
                   DFIM_ADSAO_INDVD,
                   CINDCD_EXCL_REG,
                   HINCL_REG
             FROM  DB2PRD.ADSAO_INDVD_PCOTE
             WHERE
                   CJUNC_DEPDC        = :GFCTB009.CJUNC-DEPDC  AND
                   CCTA_CLI           = :GFCTB009.CCTA-CLI     AND
                   HINCL_REG         <= :GFCTB009.HINCL-REG
             ORDER BY
                   HINCL_REG DESC
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTL9'.

       COPY 'I#GFCTLA'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE DIVISION              USING GFCTL9-ENTRADA
                                             GFCTLA-SAIDA
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
                                          GFCTLA-SAIDA.

           INITIALIZE GFCT0M-AREA-ERROS
                      GFCTLA-SAIDA.

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

           MOVE +100                   TO GFCTFZ-LL.
           MOVE ZEROS                  TO GFCTFZ-ZZ.
           MOVE GFCTL9-TRANSACAO       TO GFCTFZ-TRANSACAO.
           MOVE GFCTL9-FUNCAO          TO GFCTFZ-FUNCAO.
           MOVE GFCTL9-FUNC-BDSCO      TO GFCTFZ-FUNC-BDSCO.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE 'N'                    TO GFCTFZ-FIM.

           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTLA-ERRO
               MOVE WRK-MODULO         TO WRK-NOME-MOD
                                          GFCT0M-PROGRAMA
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0130'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 9
               MOVE 9                  TO GFCTLA-ERRO
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'GFCT0130'     TO GFCT0M-TRANSACAO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 9                  TO GFCTLA-ERRO
               MOVE 'SISTEMA INDISPONIVEL'
                                       TO GFCT0M-TEXTO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0130'         TO GFCT0M-TRANSACAO
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

RIC   *    MOVE +270                   TO GFCTLA-LL.
RIC        MOVE +310                   TO GFCTLA-LL.
           MOVE ZEROS                  TO GFCTLA-ZZ.
           MOVE GFCTL9-TRANSACAO       TO GFCTLA-TRANSACAO.
           MOVE GFCTL9-FUNCAO          TO GFCTLA-FUNCAO.
           MOVE GFCTL9-FILTRO          TO GFCTLA-FILTRO.
           MOVE GFCTL9-PONTEIRO        TO GFCTLA-PONTEIRO.
           MOVE GFCTL9-QTDE-OCOR       TO GFCTLA-QTDE-OCOR.
           MOVE GFCTL9-QTDE-TOT-REG    TO GFCTLA-QTDE-TOT-REG.
           MOVE GFCTL9-FUNC-BDSCO      TO GFCTLA-FUNC-BDSCO.

           MOVE 'N'                    TO GFCTLA-FIM.

           MOVE ZEROS                  TO GFCTLA-COD-SQL-ERRO
                                          GFCTLA-COD-MSG-ERRO
                                          GFCTLA-ERRO.

           MOVE '0000'                 TO GFCTG2-COD-MSG.

           PERFORM 1310-OBTER-DESC-MENSAGEM.

           MOVE GFCTG3-DESC-MSG        TO GFCTLA-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR DADOS                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF  GFCTL9-TRANSACAO        EQUAL LOW-VALUES   OR
               GFCTL9-TRANSACAO        EQUAL SPACES       OR
               GFCTL9-FUNCAO           EQUAL LOW-VALUES   OR
               GFCTL9-FUNCAO           EQUAL SPACES       OR
               GFCTL9-FUNC-BDSCO       EQUAL LOW-VALUES   OR
               GFCTL9-FUNC-BDSCO       EQUAL SPACES       OR
               GFCTL9-FILTRO-AGENCIA   NOT NUMERIC        OR
               GFCTL9-FILTRO-AGENCIA   EQUAL ZEROS        OR
               GFCTL9-FILTRO-CONTA     NOT NUMERIC        OR
               GFCTL9-FILTRO-CONTA     EQUAL ZEROS        OR
               GFCTL9-PONTEIRO-TARIFA  NOT NUMERIC        OR
               GFCTL9-QTDE-OCOR        NOT NUMERIC        OR
               GFCTL9-QTDE-TOT-REG     NOT NUMERIC        OR
               GFCTL9-FIM              NOT EQUAL 'S' AND 'N'
               MOVE 'S'                TO GFCTLA-FIM
               MOVE 1                  TO GFCTLA-ERRO
               MOVE ZEROS              TO GFCTLA-COD-SQL-ERRO
               MOVE 0001               TO GFCTLA-COD-MSG-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTLA-DESC-MSG-ERRO
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
           MOVE GFCTL9-TRANSACAO               TO GFCTG2-TRANSACAO.
           MOVE GFCTL9-FUNCAO                  TO GFCTG2-FUNCAO.
           MOVE GFCTL9-FUNC-BDSCO              TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                            TO GFCTG2-TIPO-PROC.
           MOVE 'N'                            TO GFCTG2-FIM.

           MOVE 'GFCT5523'                     TO WRK-MODULO.

           CALL 'POOL0081'                     USING WRK-MODULO
                                                     GFCTG2-ENTRADA
                                                     GFCTG3-SAIDA
                                                     GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                     NOT EQUAL ZEROS
               MOVE 9                          TO GFCTLA-ERRO
               MOVE WRK-MODULO                 TO GFCT0M-PROGRAMA
                                                  WRK-NOME-MOD
               MOVE WRK-MSG01                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0130'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO                     EQUAL 9
               MOVE GFCTG3-ERRO                TO GFCTLA-ERRO
               IF  GFCT0M-TIPO-ACESSO          EQUAL 'DB2'
                   MOVE GFCTG3-COD-SQL-ERRO    TO GFCTLA-COD-SQL-ERRO
                   MOVE GFCTG3-COD-MSG-ERRO    TO GFCTLA-COD-MSG-ERRO
                   MOVE GFCTG3-DESC-MSG-ERRO   TO GFCTLA-DESC-MSG-ERRO
               ELSE
                   MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
                   MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
                   MOVE GFCTG3-DESC-MSG-ERRO   TO GFCT0M-TEXTO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO                     EQUAL 1
               MOVE 'S'                        TO GFCTLA-FIM
               MOVE GFCTG3-ERRO                TO GFCTLA-ERRO
               MOVE GFCTG3-COD-SQL-ERRO        TO GFCTLA-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO        TO GFCTLA-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO       TO GFCTLA-DESC-MSG-ERRO
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
      * ABRIR CURSOR DA TABELA GFCTB009                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-ABRIR-CURSOR               SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO IND-1
                                          WRK-CSERVC-TARIF.
           MOVE SPACES                 TO WRK-FIM-CURSOR.

           MOVE GFCTL9-FILTRO-AGENCIA  TO CJUNC-DEPDC      OF GFCTB009.
           MOVE GFCTL9-FILTRO-CONTA    TO CCTA-CLI         OF GFCTB009.

           IF  GFCTL9-PONTEIRO-TIMESTAMP
                                       EQUAL SPACES
               MOVE '3333-01-01-01.01.01.000001'
                                       TO HINCL-REG        OF GFCTB009
           ELSE
               MOVE GFCTL9-PONTEIRO-TIMESTAMP
                                       TO HINCL-REG        OF GFCTB009
           END-IF.

           EXEC SQL
               OPEN CSR01-GFCTB009
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'ADSAO_INDVD_PCOTE' TO WRK-NOM-TAB
               MOVE 'OPEN  '           TO WRK-COMANDO-SQL
               MOVE '0001'             TO WRK-LOCAL
               MOVE  0005              TO GFCTLA-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB009'         TO WRK-INC-TAB
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
           DELIMITED BY '  '           INTO GFCTLA-DESC-MSG-ERRO.

           MOVE  9                     TO GFCTLA-ERRO.
           MOVE 'GFCT0130'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE  SQLCA                 TO GFCT0M-SQLCA-AREA.
           MOVE  SQLCODE               TO GFCT0M-SQLCODE
                                          GFCTLA-COD-SQL-ERRO.
           MOVE WRK-NOM-TAB            TO GFCT0M-NOME-TAB.
           MOVE WRK-COMANDO-SQL        TO GFCT0M-COMANDO-SQL.
           MOVE WRK-LOCAL              TO GFCT0M-LOCAL.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * LER CURSOR DA TABELA GFCTB009                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-LER-CURSOR                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR01-GFCTB009 INTO
                   :GFCTB009.CSERVC-TARIF,
                   :GFCTB009.DINIC-ADSAO-INDVD,
                   :GFCTB009.DFIM-ADSAO-INDVD,
                   :GFCTB009.CINDCD-EXCL-REG,
                   :GFCTB009.HINCL-REG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'ADSAO_INDVD_PCOTE' TO WRK-NOM-TAB
               MOVE 'FETCH '           TO WRK-COMANDO-SQL
               MOVE '0002'             TO WRK-LOCAL
               MOVE  0006              TO GFCTLA-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB009'         TO WRK-INC-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR
                                          GFCTLA-FIM
               IF  IND-1               EQUAL ZEROS
                   PERFORM 2400-FECHAR-CURSOR
                   MOVE 1              TO GFCTLA-ERRO
                   MOVE ZEROS          TO GFCTLA-COD-SQL-ERRO
                   MOVE 0853           TO GFCTLA-COD-MSG-ERRO
                   MOVE '0853'         TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG       TO GFCTLA-DESC-MSG-ERRO
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

RIC   *    IF  IND-1                   GREATER 10
RIC        IF  IND-1                   GREATER 7
               MOVE 'S'                TO WRK-FIM-CURSOR
               MOVE CSERVC-TARIF       OF GFCTB009
                                       TO GFCTLA-PONTEIRO-TARIFA
               MOVE HINCL-REG          OF GFCTB009
                                       TO GFCTLA-PONTEIRO-TIMESTAMP
               GO TO 2300-99-FIM
           END-IF.

RIC   *    ADD 74                      TO GFCTLA-LL.
RIC        ADD 100                     TO GFCTLA-LL.
           ADD 1                       TO GFCTLA-QTDE-TOT-REG.

           IF  CSERVC-TARIF            OF GFCTB009
                                       NOT EQUAL WRK-CSERVC-TARIF
               PERFORM 2310-ACESSAR-GFCTB0D8
               MOVE CSERVC-TARIF       OF GFCTB0D8
                                       TO WRK-CSERVC-TARIF
           END-IF.

RIC        PERFORM 2320-OBTER-DADOS-CLIENTE.

RIC        MOVE GFCTHA-NOME-CLIENTE    TO  GFCTLA-NOME-CLIENTE

           MOVE CSERVC-TARIF           OF GFCTB009
                                       TO GFCTLA-TARIFA-ADESAO(IND-1).
           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO GFCTLA-DESCR-TARIFA(IND-1).
           MOVE DINIC-ADSAO-INDVD      OF GFCTB009
                                       TO
                                        GFCTLA-DT-INI-VIG-ADESAO(IND-1).
           MOVE DFIM-ADSAO-INDVD       OF GFCTB009
                                       TO
                                        GFCTLA-DT-FIM-VIG-ADESAO(IND-1).

           IF  CINDCD-EXCL-REG         OF GFCTB009
                                       EQUAL ZEROS
               MOVE SPACES             TO GFCTLA-SITUACAO-ADESAO(IND-1)
           ELSE
               MOVE 'CANCELADA'        TO GFCTLA-SITUACAO-ADESAO(IND-1)
           END-IF.

RIC        MOVE HINCL-REG              OF GFCTB009
RIC                                    TO GFCTLA-TIMESTAMP(IND-1)

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

           MOVE CSERVC-TARIF           OF GFCTB009
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
               MOVE  0012              TO GFCTLA-COD-MSG-ERRO
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
           MOVE GFCTL9-TRANSACAO       TO GFCTG0-TRANSACAO.
           MOVE GFCTL9-FUNCAO          TO GFCTG0-FUNCAO.
           MOVE GFCTL9-FUNC-BDSCO      TO GFCTG0-FUNC-BDSCO.
           MOVE GFCTL9-FILTRO-AGENCIA  TO GFCTG0-COD-DEPDC.
           MOVE GFCTL9-FILTRO-CONTA    TO GFCTG0-CCTA-CLI.
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
               MOVE 9                  TO GFCTLA-ERRO
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
               MOVE 'S'                TO GFCTLA-FIM
               MOVE GFCTHA-ERRO        TO GFCTLA-ERRO
               MOVE GFCTHA-COD-SQL-ERRO
                                       TO GFCTLA-COD-SQL-ERRO
               MOVE GFCTHA-COD-MSG-ERRO
                                       TO GFCTLA-COD-MSG-ERRO
               IF  GFCTHA-ERRO         EQUAL 1
                   MOVE GFCTHA-DESC-MSG-ERRO
                                       TO GFCTLA-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5054-' GFCTHA-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTLA-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * FECHAR CURSOR DA TABELA GFCTB009                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-FECHAR-CURSOR              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-GFCTB009
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'ADSAO_INDVD_PCOTE' TO WRK-NOM-TAB
               MOVE 'CLOSE '           TO WRK-COMANDO-SQL
               MOVE '0004'             TO WRK-LOCAL
               MOVE  0011              TO GFCTLA-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB009'         TO WRK-INC-TAB
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
