      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0137.
       AUTHOR.     DAYANE FILAKOSKI.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0137                                    *
      *    PROGRAMADORA:   DAYANE FILAKOSKI        - CPM PATO BRANCO   *
      *    ANALISTA CPM:   ELIAS BOSCATO           - CPM PATO BRANCO   *
      *    ANALISTA....:   MARCELO                 - PROCWORK /GP.50   *
      *    DATA........:   13/04/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CONSULTA PARA CANCELAMENTO DE EXCECAO DE A- *
      *      DESAO COMPULSORIA.                                        *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *

      *                  TABLE                     INCLUDE/BOOK        *
      *                   DB2PRD.PARM_DATA_PROCM     GFCTB0A1          *
      *                   DB2PRD.EXEC_ADSAO_COMP     GFCTB048          *
      *                   DB2PRD.V01UF               DCITV002          *
TESTE *                   DB2PRD.ADSAO_COMP_PCOTE    GFCTB001          *
      *                   DB2PRD.ADSAO_GRP_DEPDC     GFCTB002          *
      *                   DB2PRD.ADSAO_GRP_MUN       GFCTB004          *
      *                   DB2PRD.ADSAO_GRP_PAB       GFCTB005          *
      *                   DB2PRD.ADSAO_GRP_PSTAL     GFCTB006          *
      *                   DB2PRD.ADSAO_GRP_SGMTO     GFCTB007          *
      *                   DB2PRD.ADSAO_GRP_UF        GFCTB008          *
      *                   DB2PRD.TADSAO_GRP_CLI      GFCTB0H9          *
      *                   DB2PRD.TTPO_SGMTO_GSTAO    GFCTB0H5          *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTOF - ENTRADA                                          *
      *    I#GFCTOG - SAIDA                                            *
      *    I#GFCTFZ - ENTRADA - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG1 - SAIDA   - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG2 - ENTRADA - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTG3 - SAIDA   - OBTER DESCRICAO DE MENSAGEM.           *

      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *    I#GFCTGB - ENTRADA - OBTER DESCRICAO DE PAB.                *
      *    I#GFCTGC - SAIDA   - OBTER DESCRICAO DE PAB.                *
      *    GFCTWABC - ENTRADA - OBTER DESCRICAO DE BANCO EXPRESSO.     *
      *    GFCTWABD - SAIDA   - OBTER DESCRICAO DE POSTAL OU EXPRESSO. *
      *    I#GFCT1V - ENTRADA - OBTER DESCRICAO DE AGENCIA.            *
      *    I#GFCT1X - SAIDA   - OBTER DESCRICAO DE AGENCIA.
      *    I#RURC88 - AREA PARA MODULO RURC9020.                       *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                  *
      *    POOL7600 - OBTER DATA E HORA DO SISTEMA.                    *
      *    GFCT5522 - VERIFICAR DISPONIBILIDADE DO ON-LINE.            *
      *    GFCT5109 - OBTER DESCRICAO DE PAB.                          *
      *    GFCT5209 - OBTER DESCRICAO DE POSTAL OU EXPRESSO.           *
      *    GFCT5006 - OBTER DESCRICAO DE AGENCIA.                      *
      *    DCIT8000 - OBTER DESCRICAO DE UF.                           *
      *    RURC9020 - OBTER DESCRICAO DE MUNICIPIO.                    *
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
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.

       01  WRK-MSG-TAB.
           05 FILLER                   PIC  X(003)         VALUE
              ' - '.
           05 WRK-NOME-TABELA          PIC  X(008)         VALUE SPACES.

       01  WRK-CSERVC-TARIF            PIC S9(005)         VALUE ZEROS.

       01  WRK-CAGPTO-CTA              PIC S9(003)         VALUE ZEROS.

       01  WRK-CSEQ-AGPTO-CTA          PIC S9(009)         VALUE ZEROS.


       01  WRK-DINIC-ADSAO-COMP        PIC  X(010)         VALUE SPACES.

       01  WRK-INI-ANTES               PIC  9(008)         VALUE ZEROS.
       01  WRK-INI-ANTES-R             REDEFINES WRK-INI-ANTES.
           05  WRK-INI-ANTES-ANO       PIC  9(004).
           05  WRK-INI-ANTES-MES       PIC  9(002).
           05  WRK-INI-ANTES-DIA       PIC  9(002).

       01  WRK-DPROCM-ATUAL            PIC  9(008)         VALUE ZEROS.
       01  WRK-DPROCM-R                REDEFINES WRK-DPROCM-ATUAL.
           05  WRK-DPROCM-ANO          PIC  9(004).
           05  WRK-DPROCM-MES          PIC  9(002).
           05  WRK-DPROCM-DIA          PIC  9(002).

       01  WRK-DPROCM-ALTERADA.
           05  WRK-DPROCM-ALT-DIA      PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  WRK-DPROCM-ALT-MES      PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  WRK-DPROCM-ALT-ANO      PIC  9(004)         VALUE ZEROS.


       01  WRK-MSG05.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA DO MODULO '.
           05 WRK-NOME-MODULO          PIC  X(008)         VALUE SPACES.

       01  WRK-DATA.
           05 WRK-DIA                  PIC  9(002)         VALUE ZEROS.
           05 WRK-MES                  PIC  9(002)         VALUE ZEROS.
           05 WRK-ANO                  PIC  9(004)         VALUE ZEROS.

       01  WRK-DATA-AUX.
           05 FILLER                   PIC  9(002)         VALUE ZEROS.
           05 WRK-DATA-1640.
             10 WRK-DIA-AUX            PIC  9(002)         VALUE ZEROS.
             10 WRK-MES-AUX            PIC  9(002)         VALUE ZEROS.
             10 WRK-ANO-AUX            PIC  9(004)         VALUE ZEROS.

       01  WRK-DATA-1640-AUX           PIC  9(008)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE

           '* AREA PARA POOL1640 *'.
      *----------------------------------------------------------------*

       01  WRK-1640-CAMPOS.
           05  WRK-1640-DATA-ENTRADA   PIC  X(010)         VALUE SPACES.
           05  WRK-1640-FORMATO-ENTRADA PIC 9(002)         VALUE ZEROS.
           05  WRK-1640-TIPO-INCREMENTO PIC X(002)         VALUE SPACES.
           05  WRK-1640-INCREMENTO     PIC  9(003)         VALUE ZEROS.
           05  WRK-1640-FORMATO-SAIDA  PIC  9(002)         VALUE ZEROS.
           05  WRK-1640-DATA-SAIDA     PIC  X(010)         VALUE SPACES.
           05  WRK-1640-MENSAGEM       PIC  X(080)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA POOL0025 *'.
      *----------------------------------------------------------------*

       01  WRK-DATA-ABERT              PIC  9(009) COMP-3  VALUE ZEROS.
       01  WRK-PRAZO                   PIC S9(005) COMP-3  VALUE ZEROS.
       01  WRK-DATA-VENC               PIC  9(009) COMP-3  VALUE ZEROS.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA INDICADORES DE NULIDADE *'.
      *----------------------------------------------------------------*

       01  WRK-GFCTB007-NULL.
           05 WRK-CSGMTO-GSTAO-TARIF-NULL
                                       PIC S9(04) COMP  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO DO MODULO. DCIT8000 *'.
      *----------------------------------------------------------------*

       01  WRK-DCIT-UNIDADE-FEDERATIVA.
           05  WRK-AREA-DCLGEN-DCITV002    PIC  X(070).
           05  WRK-DCIT-CODIGO-RETORNO     PIC  9(002).

       01  WRK-POOL7100                    PIC  X(107).
       01  WRK-SQLCA                       PIC  X(136).


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5522 - ENTRADA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTFZ'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5522 - SAIDA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5523 - ENTRADA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG2'.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5523 - SAIDA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5109 - ENTRADA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTGB'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5109 - SAIDA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTGC'.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5209 - ENTRADA *'.
      *----------------------------------------------------------------*

       COPY 'GFCTWABC'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5209 - SAIDA *'.
      *----------------------------------------------------------------*

       COPY 'GFCTWABD'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5006 - ENTRADA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCT1V'.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5006 - SAIDA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCT1X'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM RURC9020 - ENTRADA *'.
      *----------------------------------------------------------------*

       COPY 'I#RURC88'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TABELA DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB048
           END-EXEC.

           EXEC SQL
             INCLUDE DCITV002
           END-EXEC.

TESTE      EXEC SQL
TESTE        INCLUDE GFCTB001
TESTE      END-EXEC.

           EXEC SQL
             INCLUDE GFCTB002
           END-EXEC.


           EXEC SQL
             INCLUDE GFCTB004
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB005
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB006
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB007
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB008
           END-EXEC.


           EXEC SQL
             INCLUDE GFCTB0H9
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0H5
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTOF'.

       COPY 'I#GFCTOG'.


       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTOF-ENTRADA
                                                      GFCTOG-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      * ROTINA PRINCIPAL.                                              *
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
      * ROTINA PARA INICIALIZACAO DO PROGRAMA.                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTOG-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCTOG-SAIDA
                      GFCT0M-AREA-ERROS.

           PERFORM 1100-VERIFICAR-ON-LINE.

           PERFORM 1200-MONTAR-AREA-SAIDA.


           PERFORM 1300-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICA SITUACAO DO SISTEMA, LIBERADO OU BLOQUEADO.           *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-VERIFICAR-ON-LINE          SECTION.
      *----------------------------------------------------------------*

           MOVE 100                    TO GFCTFZ-LL.
           MOVE ZEROS                  TO GFCTFZ-ZZ.
           MOVE GFCTOF-TRANSACAO       TO GFCTFZ-TRANSACAO.
           MOVE GFCTOF-FUNCAO          TO GFCTFZ-FUNCAO.
           MOVE GFCTOF-FUNC-BDSCO      TO GFCTFZ-FUNC-BDSCO.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE 'N'                    TO GFCTFZ-FIM.


           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0137'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTOG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 9
               MOVE 'GFCT0137'         TO GFCT0M-TRANSACAO
               MOVE GFCTG1-COD-SQL-ERRO
                                       TO  GFCT0M-SQLCODE

                                           GFCTOG-COD-SQL-ERRO
               MOVE GFCTG1-COD-MSG-ERRO
                                       TO  GFCTOG-COD-MSG-ERRO
               MOVE GFCTG1-DESC-MSG-ERRO
                                       TO  GFCTOG-DESC-MSG-ERRO
                                           GFCT0M-TEXTO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 'SISTEMA INDISPONIVEL'
                                       TO GFCT0M-TEXTO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0137'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTOG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.

      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA INICIALIZACAO E MONTAGEM DA AREA DE SAIDA A PARTIR *
      * DA AREA DE ENTRADA.                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-MONTAR-AREA-SAIDA          SECTION.
      *----------------------------------------------------------------*

TESTE *--- INITIALIZE                  GFCTOG-SAIDA.

           MOVE 1010                   TO GFCTOG-LL.
           MOVE GFCTOF-TRANSACAO       TO GFCTOG-TRANSACAO.
           MOVE GFCTOF-FUNCAO          TO GFCTOG-FUNCAO.
           MOVE GFCTOF-FUNC-BDSCO      TO GFCTOG-FUNC-BDSCO.
           MOVE GFCTOF-TERMINAL        TO GFCTOG-TERMINAL.
           MOVE 'S'                    TO GFCTOG-FIM.
TESTE *--- MOVE GFCTOF-FIXO            TO GFCTOG-FIXO.
           MOVE GFCTOF-CAGENCIA-EXCEC  TO GFCTOG-CAGENCIA-EXCEC.
           MOVE GFCTOF-CCONTA-EXCEC    TO GFCTOG-CCONTA-EXCEC.

           MOVE GFCTOF-CPACOTE         TO GFCTOG-CPACOTE.
           MOVE GFCTOF-CAGPTO          TO GFCTOG-CAGPTO.
           MOVE GFCTOF-DINI-ADESAO     TO GFCTOG-DINI-ADESAO.
           MOVE GFCTOF-SEQUENCIA       TO GFCTOG-SEQUENCIA.
           MOVE GFCTOF-HINCL           TO GFCTOG-HINCL.
           MOVE ZEROS                  TO GFCTOG-CONTROLE.

           MOVE ZEROS                  TO GFCTOG-ERRO
                                          GFCTOG-COD-SQL-ERRO
                                          GFCTOG-COD-MSG-ERRO
                                          GFCTG2-COD-MSG.

           PERFORM 1210-OBTER-DESC-MSG.

           MOVE GFCTG3-DESC-MSG        TO GFCTOG-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************

      * ROTINA PARA ACESSO AO MODULO DE DESCRICOES DE MENSAGENS.       *
      ******************************************************************
      *----------------------------------------------------------------*
       1210-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE 100                    TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.

           MOVE GFCTOF-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTOF-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTOF-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'N'                    TO GFCTG2-FIM.

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE 'GFCT5523'             TO WRK-MODULO.


           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.


           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0137'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTOG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 9                  TO GFCTOG-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO

               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0137'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE 1                  TO GFCTOG-ERRO
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO GFCTOG-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO GFCTOG-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO GFCTOG-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************

      * ROTINA PARA CONSISTENCIA DOS DADOS DE ENTRADA.                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (GFCTOF-TRANSACAO         EQUAL SPACES OR LOW-VALUES) OR
              (GFCTOF-FUNCAO            EQUAL SPACES OR LOW-VALUES) OR
              (GFCTOF-FUNC-BDSCO        EQUAL SPACES OR LOW-VALUES) OR
TESTE         (GFCTOF-CAGENCIA-EXCEC    NOT NUMERIC  OR
TESTE          GFCTOF-CAGENCIA-EXCEC    EQUAL ZEROS)                OR
TESTE         (GFCTOF-CCONTA-EXCEC      NOT NUMERIC  OR
TESTE          GFCTOF-CCONTA-EXCEC      EQUAL ZEROS)                OR
              (GFCTOF-CPACOTE           NOT NUMERIC  OR
               GFCTOF-CPACOTE           EQUAL ZEROS)                OR
              (GFCTOF-CAGPTO            NOT EQUAL 004 AND 009 AND
                                                  014 AND 015 AND
                                                  017 AND 018 AND
                                                  019 AND 020)      OR
              (GFCTOF-DINI-ADESAO       EQUAL SPACES OR LOW-VALUES) OR
              (GFCTOF-SEQUENCIA         NOT NUMERIC  OR

               GFCTOF-SEQUENCIA         EQUAL ZEROS)                OR
TESTE         (GFCTOF-HINCL             EQUAL SPACES OR LOW-VALUES)
               MOVE  1                  TO GFCTOG-ERRO
                                           GFCTOG-COD-MSG-ERRO
               MOVE  ZEROS              TO GFCTOG-COD-SQL-ERRO
TESTE          MOVE '0001'              TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MSG
TESTE *---     MOVE  GFCTG3-DESC-MSG-ERRO TO GFCTOG-DESC-MSG-ERRO
TESTE          MOVE  GFCTG3-DESC-MSG    TO GFCTOG-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 1310-SELECIONAR-GFCTB0A1.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA QUE FAZ SELECT NA TABELA GFCTB0A1.                      *
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
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB

               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE  '0010'            TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTOG-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'GFCTB0A1'         TO WRK-NOME-TABELA
               PERFORM 1311-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA QUE MOVE O ERRO SQL.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1311-MOVER-ERRO-SQL             SECTION.
      *----------------------------------------------------------------*

           MOVE 9                      TO GFCTOG-ERRO.

           MOVE 'GFCT0137'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.
           MOVE SQLCODE                TO GFCT0M-SQLCODE
                                          GFCTOG-COD-SQL-ERRO.
           MOVE SPACES                 TO GFCTOG-DESC-MSG-ERRO.

           PERFORM 1210-OBTER-DESC-MSG.

           STRING GFCTG3-DESC-MSG
                  WRK-MSG-TAB
           DELIMITED BY '  '           INTO GFCTOG-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1311-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA DE PROCESSAMENTO PRINCIPAL DO PROGRAMA.                 *
      ******************************************************************
      *----------------------------------------------------------------*

       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-OBTER-VIGENCIA-ANTES.

           PERFORM 2200-GERAR-VIGENCIA-APOS.

           PERFORM 2300-OBTER-ARGUMENTO-DESCRICAO.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTEM A PRIMEIRA VIGENCIA ACESSANDO A TABELA GFCTB048.         *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-OBTER-VIGENCIA-ANTES       SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTOF-CAGENCIA-EXCEC  TO CJUNC-DEPDC       OF GFCTB048.

           MOVE GFCTOF-CCONTA-EXCEC    TO CCTA-CLI          OF GFCTB048.
           MOVE GFCTOF-CPACOTE         TO CSERVC-TARIF      OF GFCTB048.
           MOVE GFCTOF-CAGPTO          TO CAGPTO-CTA        OF GFCTB048.
           MOVE GFCTOF-DINI-ADESAO     TO DINIC-ADSAO-COMP  OF GFCTB048.
           MOVE GFCTOF-SEQUENCIA       TO CSEQ-AGPTO-CTA    OF GFCTB048.
           MOVE GFCTOF-HINCL           TO HINCL-EXCEC-ADSAO OF GFCTB048.

           EXEC SQL
             SELECT
                   DINIC_EXCEC_ADSAO,
                   DFIM_EXCEC_ADSAO
             INTO
                   :GFCTB048.DINIC-EXCEC-ADSAO,
                   :GFCTB048.DFIM-EXCEC-ADSAO
             FROM   DB2PRD.EXCEC_ADSAO_COMP
             WHERE
                   CSERVC_TARIF       = :GFCTB048.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB048.CAGPTO-CTA         AND
                   CSEQ_AGPTO_CTA     = :GFCTB048.CSEQ-AGPTO-CTA     AND
                   DINIC_ADSAO_COMP   = :GFCTB048.DINIC-ADSAO-COMP   AND
                   CJUNC_DEPDC        = :GFCTB048.CJUNC-DEPDC        AND

                   CCTA_CLI           = :GFCTB048.CCTA-CLI           AND
                   HINCL_EXCEC_ADSAO  = :GFCTB048.HINCL-EXCEC-ADSAO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'EXCEC_ADSAO_COMP' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTOG-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB048'         TO WRK-NOME-TABELA
               PERFORM 1311-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE DINIC-EXCEC-ADSAO OF GFCTB048
                                       TO GFCTOG-DATA-INICIO-ANTES.
           MOVE DFIM-EXCEC-ADSAO  OF GFCTB048

                                       TO GFCTOG-DATA-FIM-ANTES.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * GERA VIGENCIA POSTERIOR VERIFICANDO DATA.                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-GERAR-VIGENCIA-APOS        SECTION.
      *----------------------------------------------------------------*

           MOVE DINIC-EXCEC-ADSAO OF GFCTB048(1:2)
                                       TO WRK-INI-ANTES-DIA.
           MOVE DINIC-EXCEC-ADSAO OF GFCTB048(4:2)
                                       TO WRK-INI-ANTES-MES.
           MOVE DINIC-EXCEC-ADSAO OF GFCTB048(7:4)
                                       TO WRK-INI-ANTES-ANO.

           MOVE DPROCM-ATUAL OF GFCTB0A1(1:2)

                                       TO WRK-DPROCM-DIA.
           MOVE DPROCM-ATUAL OF GFCTB0A1(4:2)
                                       TO WRK-DPROCM-MES.
           MOVE DPROCM-ATUAL  OF GFCTB0A1(7:4)
                                       TO WRK-DPROCM-ANO.

           IF  WRK-INI-ANTES           GREATER WRK-DPROCM-ATUAL
               MOVE DINIC-EXCEC-ADSAO  TO GFCTOG-DATA-INICIO-APOS
               MOVE DFIM-EXCEC-ADSAO   TO GFCTOG-DATA-FIM-APOS
           ELSE
               PERFORM 2210-OBTER-ULTIMO-DIA
               MOVE WRK-DPROCM-DIA      TO WRK-DPROCM-ALT-DIA
               MOVE WRK-DPROCM-MES      TO WRK-DPROCM-ALT-MES
               MOVE WRK-DPROCM-ANO      TO WRK-DPROCM-ALT-ANO
               MOVE WRK-DPROCM-ALTERADA TO GFCTOG-DATA-FIM-APOS
               MOVE DINIC-EXCEC-ADSAO   OF GFCTB048
                                        TO GFCTOG-DATA-INICIO-APOS
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.

      *----------------------------------------------------------------*

      ******************************************************************
      * OBTEM ULTIMO DIA DO MES.                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-OBTER-ULTIMO-DIA           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-DPROCM-ANO         TO WRK-ANO.
           MOVE WRK-DPROCM-MES         TO WRK-MES.
           MOVE WRK-DPROCM-DIA         TO WRK-DIA.
           MOVE WRK-DATA               TO WRK-DATA-1640.
           MOVE '01'                   TO WRK-DIA-AUX.
           MOVE WRK-DATA-AUX           TO WRK-1640-DATA-ENTRADA.
           MOVE 2                      TO WRK-1640-FORMATO-ENTRADA
                                          WRK-1640-FORMATO-SAIDA.
           MOVE 'M'                    TO WRK-1640-TIPO-INCREMENTO.
           MOVE 1                      TO WRK-1640-INCREMENTO.

           CALL 'POOL1640'             USING WRK-1640-DATA-ENTRADA

                                             WRK-1640-FORMATO-ENTRADA
                                             WRK-1640-TIPO-INCREMENTO
                                             WRK-1640-INCREMENTO
                                             WRK-1640-FORMATO-SAIDA
                                             WRK-1640-DATA-SAIDA
                                             WRK-1640-MENSAGEM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
TESTE *---     MOVE 1                  TO GFCTOG-ERRO
TESTE *---     MOVE ZEROS              TO GFCTOG-COD-SQL-ERRO
TESTE *---     MOVE WRK-1640-MENSAGEM
TESTE *---                             TO GFCTOG-DESC-MSG-ERRO
TESTE          MOVE 'POOL1640'         TO GFCT0M-PROGRAMA
TESTE                                     WRK-NOME-MODULO
TESTE          MOVE WRK-MSG05          TO GFCT0M-TEXTO
TESTE          MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
TESTE          MOVE 'GFCT0137'         TO GFCT0M-TRANSACAO
TESTE          MOVE 9                  TO GFCTOG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.


           MOVE WRK-1640-DATA-SAIDA(3:8) TO WRK-DATA-1640-AUX.
           MOVE WRK-DATA-1640-AUX        TO WRK-DATA-ABERT.
           MOVE -1                       TO WRK-PRAZO.

           CALL 'POOL0025'             USING WRK-DATA-ABERT
                                             WRK-PRAZO
                                             WRK-DATA-VENC.

           IF  RETURN-CODE             NOT EQUAL ZEROS
TESTE *---     MOVE 1                  TO GFCTOG-ERRO
TESTE *---     MOVE ZEROS              TO GFCTOG-COD-SQL-ERRO
TESTE *---     MOVE 'ERRO NO ACESSO A POOL0025'
TESTE *---                             TO GFCTOG-DESC-MSG-ERRO
TESTE          MOVE 'POOL0025'         TO GFCT0M-PROGRAMA
TESTE                                     WRK-NOME-MODULO
TESTE          MOVE WRK-MSG05          TO GFCT0M-TEXTO
TESTE          MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
TESTE          MOVE 'GFCT0137'         TO GFCT0M-TRANSACAO
TESTE          MOVE 9                  TO GFCTOG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.


           MOVE WRK-DATA-VENC          TO WRK-DATA-1640-AUX.
           MOVE WRK-DATA-1640-AUX      TO WRK-DATA-1640.
           MOVE WRK-DIA-AUX            TO WRK-DPROCM-DIA.
           MOVE WRK-MES-AUX            TO WRK-DPROCM-MES.
           MOVE WRK-ANO-AUX            TO WRK-DPROCM-ANO.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OTEM O ARQGUMENTO DA DESCRICAO.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-OBTER-ARGUMENTO-DESCRICAO  SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTOF-CAGPTO
               WHEN 15
                   PERFORM 2310-CONSULTAR-PAB

               WHEN 14
                   PERFORM 2320-CONSULTAR-AGENCIA
               WHEN 17
                   PERFORM 2330-CONS-POSTAL-EXPRE
               WHEN 20
                   PERFORM 2330-CONS-POSTAL-EXPRE
               WHEN 4
                   PERFORM 2340-CONSULTAR-CLIENTE
               WHEN 9
                   PERFORM 2350-CONSULTAR-SEGMENTO
               WHEN 18
                   PERFORM 2360-CONSULTAR-MUNICIPIO
               WHEN 19
                   PERFORM 2370-CONSULTAR-UF
           END-EVALUATE.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************

      * CONSULTA A TABELA PAB - GFCTB005.                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2310-CONSULTAR-PAB              SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTOF-CPACOTE         TO CSERVC-TARIF     OF GFCTB005.
           MOVE GFCTOF-CAGPTO          TO CAGPTO-CTA       OF GFCTB005.
           MOVE GFCTOF-DINI-ADESAO     TO DINIC-ADSAO-COMP OF GFCTB005.
           MOVE GFCTOF-SEQUENCIA       TO CSEQ-AGPTO-CTA   OF GFCTB005.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
             INTO

                   :GFCTB005.CSERVC-TARIF,
                   :GFCTB005.CAGPTO-CTA,
                   :GFCTB005.CSEQ-AGPTO-CTA,
                   :GFCTB005.DINIC-ADSAO-COMP,
                   :GFCTB005.CEMPR-INC,
                   :GFCTB005.CDEPDC,
                   :GFCTB005.CPOSTO-SERVC
             FROM   DB2PRD.ADSAO_GRP_PAB
             WHERE
                   CSERVC_TARIF       = :GFCTB005.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB005.CAGPTO-CTA         AND
                   CSEQ_AGPTO_CTA     = :GFCTB005.CSEQ-AGPTO-CTA     AND
                   DINIC_ADSAO_COMP   = :GFCTB005.DINIC-ADSAO-COMP
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'ADSAO_GRP_PAB'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL

               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTOG-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB005'         TO WRK-NOME-TABELA
               PERFORM 1311-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE CPOSTO-SERVC  OF GFCTB005
                                       TO GFCTOG-CPAB.

           IF  CPOSTO-SERVC OF GFCTB005 EQUAL ZEROS
               MOVE 'TODOS'            TO GFCTOG-RPAB
           ELSE
               PERFORM 2311-OBTER-DESC-PAB
               MOVE GFCTGC-DESC-POSTO(1)
                                       TO GFCTOG-RPAB
           END-IF.

           MOVE CDEPDC OF GFCTB005     TO GFCTOG-CAGENCIA.
           MOVE SPACES                 TO GFCT1V-ENTRADA.

           MOVE CDEPDC OF GFCTB005     TO GFCT1V-COD-DEPDC(1).
           PERFORM 2312-OBTER-DESC-DEPDC.
           MOVE GFCT1X-DESC-DEPDC(1)   TO GFCTOG-RAGENCIA.

TESTE      MOVE CSERVC-TARIF     OF GFCTB005
TESTE                                  TO WRK-CSERVC-TARIF.
TESTE      MOVE CAGPTO-CTA       OF GFCTB005
TESTE                                  TO WRK-CAGPTO-CTA.
TESTE      MOVE CSEQ-AGPTO-CTA   OF GFCTB005
TESTE                                  TO WRK-CSEQ-AGPTO-CTA.
TESTE      MOVE DINIC-ADSAO-COMP OF GFCTB005
TESTE                                  TO WRK-DINIC-ADSAO-COMP.
TESTE
TESTE      PERFORM 2313-CONSULTAR-AGRUP-TOTAL.
TESTE
TESTE      MOVE CINDCD-AGPTO-TOT  OF GFCTB001
TESTE                                  TO GFCTOG-TOTAL.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      ******************************************************************
      * OTEM O ARQGUMENTO DA DESCRICAO.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2311-OBTER-DESC-PAB             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA.
           MOVE 100                    TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.

           MOVE GFCTOF-TRANSACAO       TO GFCTGB-TRANSACAO.
           MOVE GFCTOF-FUNCAO          TO GFCTGB-FUNCAO.
           MOVE ZEROS                  TO GFCTGB-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTGB-QTDE-TOT-REG.
           MOVE GFCTOF-FUNC-BDSCO      TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTGB-FIM.

           MOVE CDEPDC                 OF GFCTB005
                                       TO GFCTGB-COD-DEPDC.

           MOVE CPOSTO-SERVC           OF GFCTB005
                                       TO GFCTGB-COD-POSTO.

           MOVE SPACES                 TO GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCTGC-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5109'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTGB-ENTRADA
                                             GFCTGC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO

               MOVE 'GFCT0137'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTOG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTGC-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTOG-FIM
               MOVE GFCTGC-ERRO        TO GFCTOG-ERRO
               MOVE GFCTGC-COD-SQL-ERRO
                                       TO GFCTOG-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO
                                       TO GFCTOG-COD-MSG-ERRO
               IF  GFCTGC-ERRO         EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO
                                       TO GFCTOG-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5109-'
                           GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTOG-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR

           END-IF.

      *----------------------------------------------------------------*
       2311-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTEM DESCRICAO DO DEPDC.                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2312-OBTER-DESC-DEPDC           SECTION.
      *----------------------------------------------------------------*

           MOVE 660                    TO GFCT1V-LL.
           MOVE ZEROS                  TO GFCT1V-ZZ.

           MOVE GFCTOF-TRANSACAO       TO GFCT1V-TRANSACAO.
           MOVE GFCTOF-FUNCAO          TO GFCT1V-FUNCAO.
           MOVE 001                    TO GFCT1V-QTDE-OCOR.
           MOVE ZEROS                  TO GFCT1V-QTDE-TOT-REG.
           MOVE GFCTOF-FUNC-BDSCO      TO GFCT1V-FUNC-BDSCO.

           MOVE 'N'                    TO GFCT1V-FIM.

           MOVE SPACES                 TO GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCT1X-SAIDA
                                       GFCT0M-AREA-ERROS.
           MOVE 'GFCT5006'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCT1V-ENTRADA
                                             GFCT1X-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0137'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTOG-ERRO

               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCT1X-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTOG-FIM
               MOVE GFCT1X-ERRO        TO GFCTOG-ERRO
               MOVE GFCT1X-COD-SQL-ERRO
                                       TO GFCTOG-COD-SQL-ERRO
               MOVE GFCT1X-COD-MSG-ERRO
                                       TO GFCTOG-COD-MSG-ERRO
               IF  GFCT1X-ERRO         EQUAL 1
                   MOVE GFCT1X-DESC-MSG-ERRO
                                       TO GFCTOG-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT50006-'
                           GFCT1X-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTOG-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.


      *----------------------------------------------------------------*
       2312-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

TESTE ******************************************************************
TESTE * CONSULTA A TABELA DE ADESAO COMPULSORIA DE PACOTE - GFCTB001   *
TESTE ******************************************************************
TESTE *----------------------------------------------------------------*
TESTE  2313-CONSULTAR-AGRUP-TOTAL      SECTION.
TESTE *----------------------------------------------------------------*
TESTE
TESTE      MOVE WRK-CSERVC-TARIF       TO CSERVC-TARIF     OF GFCTB001.
TESTE      MOVE WRK-CAGPTO-CTA         TO CAGPTO-CTA       OF GFCTB001.
TESTE      MOVE WRK-DINIC-ADSAO-COMP   TO DINIC-ADSAO-COMP OF GFCTB001.
TESTE      MOVE WRK-CSEQ-AGPTO-CTA     TO CSEQ-AGPTO-CTA   OF GFCTB001.
TESTE
TESTE      EXEC SQL
TESTE        SELECT
TESTE               CINDCD_AGPTO_TOT
TESTE        INTO
TESTE              :GFCTB001.CINDCD-AGPTO-TOT

TESTE        FROM   DB2PRD.ADSAO_COMP_PCOTE
TESTE        WHERE
TESTE              CSERVC_TARIF       = :GFCTB001.CSERVC-TARIF       AND
TESTE              CAGPTO_CTA         = :GFCTB001.CAGPTO-CTA         AND
TESTE              CSEQ_AGPTO_CTA     = :GFCTB001.CSEQ-AGPTO-CTA     AND
TESTE              DINIC_ADSAO_COMP   = :GFCTB001.DINIC-ADSAO-COMP
TESTE      END-EXEC.
TESTE
TESTE      IF (SQLCODE                 NOT EQUAL ZEROS) OR
TESTE         (SQLWARN0                EQUAL 'W'      )
TESTE          MOVE SPACES             TO GFCT0M-AREA-ERROS
TESTE          INITIALIZE              GFCT0M-AREA-ERROS
TESTE          MOVE 'ADSAO_COMP_PCOTE' TO GFCT0M-NOME-TAB
TESTE          MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
TESTE          MOVE '0030'             TO GFCT0M-LOCAL
TESTE          MOVE 0012               TO GFCTOG-COD-MSG-ERRO
TESTE          MOVE '0012'             TO GFCTG2-COD-MSG
TESTE          MOVE 'GFCTB001'         TO WRK-NOME-TABELA
TESTE          PERFORM 1311-MOVER-ERRO-SQL
TESTE          PERFORM 3000-FINALIZAR
TESTE      END-IF.

TESTE
TESTE *----------------------------------------------------------------*
TESTE  2313-99-FIM.                    EXIT.
TESTE *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONSULTAR AGENCIA.                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2320-CONSULTAR-AGENCIA          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTOF-CPACOTE         TO CSERVC-TARIF     OF GFCTB002.
           MOVE GFCTOF-CAGPTO          TO CAGPTO-CTA       OF GFCTB002.
           MOVE GFCTOF-DINI-ADESAO     TO DINIC-ADSAO-COMP OF GFCTB002.
           MOVE GFCTOF-SEQUENCIA       TO CSEQ-AGPTO-CTA   OF GFCTB002.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,

                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
                   CEMPR_INC,
                   CDEPDC
             INTO
                   :GFCTB002.CSERVC-TARIF,
                   :GFCTB002.CAGPTO-CTA,
                   :GFCTB002.CSEQ-AGPTO-CTA,
                   :GFCTB002.DINIC-ADSAO-COMP,
                   :GFCTB002.CEMPR-INC,
                   :GFCTB002.CDEPDC
             FROM   DB2PRD.ADSAO_GRP_DEPDC
             WHERE
                   CSERVC_TARIF       = :GFCTB002.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB002.CAGPTO-CTA         AND
                   CSEQ_AGPTO_CTA     = :GFCTB002.CSEQ-AGPTO-CTA     AND
                   DINIC_ADSAO_COMP   = :GFCTB002.DINIC-ADSAO-COMP
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )

               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'ADSAO_GRP_DEPDC'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTOG-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB002'         TO WRK-NOME-TABELA
               PERFORM 1311-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE CDEPDC OF GFCTB002     TO GFCTOG-CAGENCIA.
           MOVE SPACES                 TO GFCT1V-ENTRADA.
           MOVE CDEPDC OF GFCTB002     TO GFCT1V-COD-DEPDC(1).
           PERFORM 2312-OBTER-DESC-DEPDC.
           MOVE GFCT1X-DESC-DEPDC(1)   TO GFCTOG-RAGENCIA.

TESTE      MOVE CSERVC-TARIF     OF GFCTB002
TESTE                                  TO WRK-CSERVC-TARIF.
TESTE      MOVE CAGPTO-CTA       OF GFCTB002

TESTE                                  TO WRK-CAGPTO-CTA.
TESTE      MOVE CSEQ-AGPTO-CTA   OF GFCTB002
TESTE                                  TO WRK-CSEQ-AGPTO-CTA.
TESTE      MOVE DINIC-ADSAO-COMP OF GFCTB002
TESTE                                  TO WRK-DINIC-ADSAO-COMP.
TESTE
TESTE      PERFORM 2313-CONSULTAR-AGRUP-TOTAL.
TESTE
TESTE      MOVE CINDCD-AGPTO-TOT  OF GFCTB001
TESTE                                  TO GFCTOG-TOTAL.

      *----------------------------------------------------------------*
       2320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONSULTAR POSTAL.                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2330-CONS-POSTAL-EXPRE          SECTION.
      *----------------------------------------------------------------*


           MOVE GFCTOF-CPACOTE         TO CSERVC-TARIF      OF GFCTB006.
           MOVE GFCTOF-CAGPTO          TO CAGPTO-CTA       OF GFCTB006.
           MOVE GFCTOF-DINI-ADESAO     TO DINIC-ADSAO-COMP OF GFCTB006.
           MOVE GFCTOF-SEQUENCIA       TO CSEQ-AGPTO-CTA   OF GFCTB006.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
             INTO
                   :GFCTB006.CSERVC-TARIF,
                   :GFCTB006.CAGPTO-CTA,
                   :GFCTB006.CSEQ-AGPTO-CTA,
                   :GFCTB006.DINIC-ADSAO-COMP,
                   :GFCTB006.CEMPR-INC,

                   :GFCTB006.CDEPDC,
                   :GFCTB006.CPOSTO-SERVC
             FROM   DB2PRD.ADSAO_GRP_PSTAL
             WHERE
                   CSERVC_TARIF       = :GFCTB006.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB006.CAGPTO-CTA         AND
                   CSEQ_AGPTO_CTA     = :GFCTB006.CSEQ-AGPTO-CTA     AND
                   DINIC_ADSAO_COMP   = :GFCTB006.DINIC-ADSAO-COMP
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'ADSAO_GRP_PAB'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTOG-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB006'         TO WRK-NOME-TABELA
               PERFORM 1311-MOVER-ERRO-SQL

               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE CPOSTO-SERVC OF GFCTB006
                                       TO GFCTOG-CPAB.
           IF  CPOSTO-SERVC OF GFCTB006 EQUAL ZEROS
               MOVE 'TODOS'            TO GFCTOG-RPAB
           ELSE
               PERFORM 2331-OBTER-DESC-POSTAL
               MOVE GFCTGC-DESC-POSTO(1)
                                       TO   GFCTOG-RPAB
           END-IF.

           MOVE CDEPDC OF GFCTB006     TO GFCTOG-CAGENCIA.

           IF  CDEPDC OF GFCTB006      EQUAL ZEROS
               MOVE 'TODOS'            TO GFCTOG-RAGENCIA
           ELSE
               MOVE SPACES             TO GFCT1V-ENTRADA
               MOVE CDEPDC OF GFCTB006 TO GFCT1V-COD-DEPDC(1)
               PERFORM 2312-OBTER-DESC-DEPDC

               MOVE GFCT1X-DESC-DEPDC(1) TO GFCTOG-RAGENCIA
           END-IF.

TESTE      MOVE CSERVC-TARIF     OF GFCTB006
TESTE                                  TO WRK-CSERVC-TARIF.
TESTE      MOVE CAGPTO-CTA       OF GFCTB006
TESTE                                  TO WRK-CAGPTO-CTA.
TESTE      MOVE CSEQ-AGPTO-CTA   OF GFCTB006
TESTE                                  TO WRK-CSEQ-AGPTO-CTA.
TESTE      MOVE DINIC-ADSAO-COMP OF GFCTB006
TESTE                                  TO WRK-DINIC-ADSAO-COMP.
TESTE
TESTE      PERFORM 2313-CONSULTAR-AGRUP-TOTAL.
TESTE
TESTE      MOVE CINDCD-AGPTO-TOT  OF GFCTB001
TESTE                                  TO GFCTOG-TOTAL.

      *----------------------------------------------------------------*
       2330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      ******************************************************************
      * OBTEM DESCRICAO DO POSTAL.                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2331-OBTER-DESC-POSTAL          SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WABC-ENTRADA.
           MOVE 100                    TO WABC-LL.
           MOVE ZEROS                  TO WABC-ZZ.

           MOVE GFCTOF-TRANSACAO       TO WABC-TRANSACAO.
           MOVE GFCTOF-FUNCAO          TO WABC-FUNCAO.
           MOVE ZEROS                  TO WABC-QTDE-OCOR.
           MOVE ZEROS                  TO WABC-QTDE-TOT-REG.
           MOVE GFCTOF-FUNC-BDSCO      TO WABC-FUNC-BDSCO.
           MOVE 'N'                    TO WABC-FIM.

           MOVE CDEPDC                 OF GFCTB006
                                       TO WABC-COD-DEPDC.
           MOVE CPOSTO-SERVC           OF GFCTB006

                                       TO WABC-COD-POSTO.
           MOVE CAGPTO-CTA             OF GFCTB006
                                       TO WABC-COD-AGRUP.

           MOVE SPACES                 TO WABD-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  WABD-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5209'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WABC-ENTRADA
                                             WABD-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO

               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0137'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTOG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WABD-ERRO             NOT EQUAL ZEROS
               MOVE 'S'              TO GFCTOG-FIM
               MOVE WABD-ERRO        TO GFCTOG-ERRO
               MOVE WABD-COD-SQL-ERRO
                                     TO GFCTOG-COD-SQL-ERRO
               MOVE WABD-COD-MSG-ERRO
                                     TO GFCTOG-COD-MSG-ERRO
               IF  WABD-ERRO         EQUAL 1
                   MOVE WABD-DESC-MSG-ERRO
                                     TO GFCTOG-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5209-'
                           WABD-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTOG-DESC-MSG-ERRO
               END-IF

               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2331-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONSULTAR CLIENTE.                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2340-CONSULTAR-CLIENTE          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTOF-CPACOTE         TO CSERVC-TARIF     OF GFCTB0H9.
           MOVE GFCTOF-CAGPTO          TO CAGPTO-CTA       OF GFCTB0H9.
           MOVE GFCTOF-DINI-ADESAO     TO DINIC-ADSAO-COMP OF GFCTB0H9.
           MOVE GFCTOF-SEQUENCIA       TO CSEQ-AGPTO-CTA   OF GFCTB0H9.

           EXEC SQL
             SELECT

                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
ST2506*            CCGC_CPF,
ST2506*            CCTRL_CPF_CGC,
ST2506*            CFLIAL_CGC
ST2506             CCGC_CPF_ST,
ST2506             CCTRL_CPF_CGC_ST,
ST2506             CFLIAL_CGC_ST
             INTO
                   :GFCTB0H9.CSERVC-TARIF,
                   :GFCTB0H9.CAGPTO-CTA,
                   :GFCTB0H9.CSEQ-AGPTO-CTA,
                   :GFCTB0H9.DINIC-ADSAO-COMP,
ST2506*            :GFCTB0H9.CCGC-CPF,
ST2506*            :GFCTB0H9.CCTRL-CPF-CGC,
ST2506*            :GFCTB0H9.CFLIAL-CGC
                   :GFCTB0H9.CCGC-CPF-ST,
                   :GFCTB0H9.CCTRL-CPF-CGC-ST,
                   :GFCTB0H9.CFLIAL-CGC-ST                  
             FROM   DB2PRD.TADSAO_GRP_CLI
             WHERE
                   CSERVC_TARIF       = :GFCTB0H9.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB0H9.CAGPTO-CTA         AND
                   CSEQ_AGPTO_CTA     = :GFCTB0H9.CSEQ-AGPTO-CTA     AND
                   DINIC_ADSAO_COMP   = :GFCTB0H9.DINIC-ADSAO-COMP

           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'TADSAO_GRP_CLI'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTOG-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0H9'         TO WRK-NOME-TABELA
               PERFORM 1311-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

ST2506*    M0VE CCGC-CPF      OF GFCTB0H9
ST2506     MOVE CCGC-CPF-ST   OF GFCTB0H9
ST2506                                 TO GFCTOG-CGC-CPF.
ST2506*    M0VE CCTRL-CPF-CGC OF GFCTB0H9
ST2506     MOVE CCTRL-CPF-CGC-ST OF GFCTB0H9
                                       TO GFCTOG-CONTROLE.

ST2506*    M0VE CFLIAL-CGC    OF GFCTB0H9
ST2506     MOVE CFLIAL-CGC-ST    OF GFCTB0H9
                                       TO GFCTOG-FILIAL.

      *----------------------------------------------------------------*
       2340-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONSULTAR SEGMENTO.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2350-CONSULTAR-SEGMENTO         SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTOF-CPACOTE         TO CSERVC-TARIF     OF GFCTB007.
           MOVE GFCTOF-CAGPTO          TO CAGPTO-CTA       OF GFCTB007.
           MOVE GFCTOF-DINI-ADESAO     TO DINIC-ADSAO-COMP OF GFCTB007.
           MOVE GFCTOF-SEQUENCIA       TO CSEQ-AGPTO-CTA   OF GFCTB007.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,

                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
                   CSGMTO_GSTAO_TARIF
             INTO
                   :GFCTB007.CSERVC-TARIF,
                   :GFCTB007.CAGPTO-CTA,
                   :GFCTB007.CSEQ-AGPTO-CTA,
                   :GFCTB007.DINIC-ADSAO-COMP,
                   :GFCTB007.CSGMTO-GSTAO-TARIF
                     :WRK-CSGMTO-GSTAO-TARIF-NULL
             FROM   DB2PRD.ADSAO_GRP_SGMTO
             WHERE
                   CSERVC_TARIF       = :GFCTB007.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB007.CAGPTO-CTA         AND
                   CSEQ_AGPTO_CTA     = :GFCTB007.CSEQ-AGPTO-CTA     AND
                   DINIC_ADSAO_COMP   = :GFCTB007.DINIC-ADSAO-COMP
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )

               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'ADSAO_GRP_SGMTO'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTOG-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB007'         TO WRK-NOME-TABELA
               PERFORM 1311-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE CSGMTO-GSTAO-TARIF OF GFCTB007
                                       TO GFCTOG-CSEGMENTO.

           PERFORM 2351-OBTER-DESC-SEGMENTO.

           MOVE ISGMTO-GSTAO-TARIF OF GFCTB0H5
                                       TO GFCTOG-RSEGMENTO.

      *----------------------------------------------------------------*

       2350-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTEM DESCRICAO DO SEGMENTO.                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2351-OBTER-DESC-SEGMENTO        SECTION.
      *----------------------------------------------------------------*

           MOVE CSGMTO-GSTAO-TARIF OF GFCTB007
                                      TO CSGMTO-GSTAO-TARIF OF GFCTB0H5.
           EXEC SQL
             SELECT
                   ISGMTO_GSTAO_TARIF
             INTO
                   :GFCTB0H5.ISGMTO-GSTAO-TARIF
             FROM   DB2PRD.TTPO_SGMTO_GSTAO
             WHERE
                   CSGMTO_GSTAO_TARIF = :GFCTB0H5.CSGMTO-GSTAO-TARIF
           END-EXEC.


           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'TTPO_SGMTO_GSTAO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTOG-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0H5'         TO WRK-NOME-TABELA
               PERFORM 1311-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2351-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONSULTAR MUNICIPIO.                               *

      ******************************************************************
      *----------------------------------------------------------------*
       2360-CONSULTAR-MUNICIPIO        SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTOF-CPACOTE         TO CSERVC-TARIF     OF GFCTB004.
           MOVE GFCTOF-CAGPTO          TO CAGPTO-CTA       OF GFCTB004.
           MOVE GFCTOF-DINI-ADESAO     TO DINIC-ADSAO-COMP OF GFCTB004.
           MOVE GFCTOF-SEQUENCIA       TO CSEQ-AGPTO-CTA   OF GFCTB004.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
                   CMUN_IBGE
             INTO
                   :GFCTB004.CSERVC-TARIF,
                   :GFCTB004.CAGPTO-CTA,
                   :GFCTB004.CSEQ-AGPTO-CTA,

                   :GFCTB004.DINIC-ADSAO-COMP,
                   :GFCTB004.CMUN-IBGE
             FROM   DB2PRD.ADSAO_GRP_MUN
             WHERE
                   CSERVC_TARIF       = :GFCTB004.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB004.CAGPTO-CTA         AND
                   CSEQ_AGPTO_CTA     = :GFCTB004.CSEQ-AGPTO-CTA     AND
                   DINIC_ADSAO_COMP   = :GFCTB004.DINIC-ADSAO-COMP
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'ADSAO_GRP_MUN'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0090'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTOG-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB004'         TO WRK-NOME-TABELA
               PERFORM 1311-MOVER-ERRO-SQL

               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE CMUN-IBGE OF GFCTB004  TO GFCTOG-CMUNIC.
           PERFORM 2361-OBTER-DESC-MUNICIPIO.
           MOVE NOME-MUNIC-RURC88      TO GFCTOG-RMUNIC.

      *----------------------------------------------------------------*
       2360-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTEM DESCRICAO DO MUNICIPIO.                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2361-OBTER-DESC-MUNICIPIO       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  AREA-RURC88.
           MOVE CMUN-IBGE OF GFCTB004  TO COD-MUNIC-RURC88.
           MOVE 'RURC9020'             TO WRK-MODULO.


           CALL 'POOL0081'             USING   WRK-MODULO
                                               AREA-RURC88.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTOG-ERRO
               MOVE ZEROS              TO GFCTOG-COD-SQL-ERRO
               MOVE 732                TO GFCTOG-COD-MSG-ERRO
               MOVE '0732'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MSG
TESTE *---     MOVE GFCTG3-DESC-MSG-ERRO
TESTE          MOVE GFCTG3-DESC-MSG    TO GFCTOG-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 10
               MOVE 1                  TO GFCTOG-ERRO
               MOVE ZEROS              TO GFCTOG-COD-SQL-ERRO
               MOVE  675               TO GFCTOG-COD-MSG-ERRO
               MOVE '0675'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MSG

TESTE *---     MOVE GFCTG3-DESC-MSG-ERRO
TESTE          MOVE GFCTG3-DESC-MSG    TO GFCTOG-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 20
               MOVE 1                  TO GFCTOG-ERRO
               MOVE ZEROS              TO GFCTOG-COD-SQL-ERRO
               MOVE 733                TO GFCTOG-COD-MSG-ERRO
               MOVE '0733'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MSG
TESTE *---     MOVE GFCTG3-DESC-MSG-ERRO
TESTE          MOVE GFCTG3-DESC-MSG    TO GFCTOG-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 99
               MOVE 1                  TO GFCTOG-ERRO
               MOVE ZEROS              TO GFCTOG-COD-SQL-ERRO
               MOVE 734                TO GFCTOG-COD-MSG-ERRO
               MOVE '0734'             TO GFCTG2-COD-MSG

               PERFORM 1210-OBTER-DESC-MSG
TESTE *---     MOVE GFCTG3-DESC-MSG-ERRO
TESTE          MOVE GFCTG3-DESC-MSG    TO GFCTOG-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2361-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONSULTAR UF.                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2370-CONSULTAR-UF               SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTOF-CPACOTE         TO CSERVC-TARIF     OF GFCTB008.
           MOVE GFCTOF-CAGPTO          TO CAGPTO-CTA       OF GFCTB008.
           MOVE GFCTOF-DINI-ADESAO     TO DINIC-ADSAO-COMP OF GFCTB008.
           MOVE GFCTOF-SEQUENCIA       TO CSEQ-AGPTO-CTA   OF GFCTB008.


           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
                   CSGL_UF
             INTO
                   :GFCTB008.CSERVC-TARIF,
                   :GFCTB008.CAGPTO-CTA,
                   :GFCTB008.CSEQ-AGPTO-CTA,
                   :GFCTB008.DINIC-ADSAO-COMP,
                   :GFCTB008.CSGL-UF
             FROM   DB2PRD.ADSAO_GRP_UF
             WHERE
                   CSERVC_TARIF       = :GFCTB008.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB008.CAGPTO-CTA         AND
                   CSEQ_AGPTO_CTA     = :GFCTB008.CSEQ-AGPTO-CTA     AND
                   DINIC_ADSAO_COMP   = :GFCTB008.DINIC-ADSAO-COMP
           END-EXEC.


           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'ADSAO_GRP_UF'     TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0100'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTOG-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB008'         TO WRK-NOME-TABELA
               PERFORM 1311-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE CSGL-UF OF GFCTB008    TO GFCTOG-CUF.
           PERFORM 2371-OBTER-DESC-UF.
           MOVE IUF     OF DCITV002    TO GFCTOG-RUF.

      *----------------------------------------------------------------*
       2370-99-FIM.                    EXIT.

      *----------------------------------------------------------------*

      ******************************************************************
      * OBTEM DESCRICAO DE UF.                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2371-OBTER-DESC-UF              SECTION.
      *----------------------------------------------------------------*

           MOVE CSGL-UF OF GFCTB008    TO CSGL-UF OF DCITV002.
           MOVE DCITV002               TO WRK-AREA-DCLGEN-DCITV002.
           MOVE 'DCIT8000'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-DCIT-UNIDADE-FEDERATIVA
                                             WRK-POOL7100
                                             WRK-SQLCA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTOG-ERRO
               MOVE ZEROS              TO GFCTOG-COD-SQL-ERRO

               MOVE 735                TO GFCTOG-COD-MSG-ERRO
               MOVE '0735'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MSG
TESTE *---     MOVE GFCTG3-DESC-MSG-ERRO
TESTE          MOVE GFCTG3-DESC-MSG    TO GFCTOG-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 1
               MOVE 1                  TO GFCTOG-ERRO
               MOVE ZEROS              TO GFCTOG-COD-SQL-ERRO
               MOVE 736                TO GFCTOG-COD-MSG-ERRO
               MOVE '0736'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MSG
TESTE *---     MOVE GFCTG3-DESC-MSG-ERRO
TESTE          MOVE GFCTG3-DESC-MSG    TO GFCTOG-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 2
               MOVE 1                  TO GFCTOG-ERRO

               MOVE ZEROS              TO GFCTOG-COD-SQL-ERRO
               MOVE 737                TO GFCTOG-COD-MSG-ERRO
               MOVE '0737'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MSG
TESTE *---     MOVE GFCTG3-DESC-MSG-ERRO
TESTE          MOVE GFCTG3-DESC-MSG    TO GFCTOG-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 3
               MOVE 1                  TO GFCTOG-ERRO
               MOVE ZEROS              TO GFCTOG-COD-SQL-ERRO
               MOVE 738                TO GFCTOG-COD-MSG-ERRO
               MOVE '0738'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MSG
TESTE *---     MOVE GFCTG3-DESC-MSG-ERRO
TESTE          MOVE GFCTG3-DESC-MSG    TO GFCTOG-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 4

               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'DCITV002'         TO WRK-NOME-TABELA
               MOVE 'V01_UF'           TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0110'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTOG-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 9                  TO GFCTOG-ERRO
               MOVE 'GFCT0137'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE WRK-SQLCA          TO SQLCA
                                          GFCT0M-SQLCA-AREA
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTOG-COD-SQL-ERRO
               MOVE SPACES             TO GFCTOG-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE WRK-AREA-DCLGEN-DCITV002
                                       TO DCITV002.


      *----------------------------------------------------------------*
       2371-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA FINALIZACAO DO PROGRAMA.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
