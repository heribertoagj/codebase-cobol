      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0133.
       AUTHOR.     MARCELO FARIA.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0133                                    *
      *    PROGRAMADOR.:   MARCELO FARIA            - CPM PATO BRANCO  *
      *    ANALISTA CPM:   JOSSIANE GAZZONI         - CPM PATO BRANCO  *
      *    ANALISTA....:   MARCELO CREMM            - PROCKWORK GP/50  *
      *    DATA........:   22/03/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   REALIZAR CONSISTENCIA DE  DATAS  VINDAS  DA *
      *       AREA DE ENTRADA, PARA REALIZAR EXCLUSAO FISICA OU  LOGI- *
      *       DAS TABELAS CORRESPONDENTES.                             *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                         INCLUDE/BOOK    *
      *                    DB2PRD.PARM_DATA_PROCM        GFCTB0A1      *
      *                    DB2PRD.ADSAO_COMP_PCOTE       GFCTB001      *
      *                    DB2PRD.ADSAO_GRP_DEPDC        GFCTB002      *
      *                    DB2PRD.ADSAO_GRP_EMPR         GFCTB003      *
      *                    DB2PRD.ADSAO_GRP_MUN          GFCTB004      *
      *                    DB2PRD.ADSAO_GRP_PAB          GFCTB005      *
      *                    DB2PRD.ADSAO_GRP_PSTAL        GFCTB006      *
      *                    DB2PRD.ADSAO_GRP_SGMTO        GFCTB007      *
      *                    DB2PRD.ADSAO_GRP_UF           GFCTB008      *
      *                    DB2PRD.ADSAO_GRP_CLI          GFCTB0H9      *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#GFCTLJ - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCTLK - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCTFZ - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5522 *
      *    I#GFCTG1 - AREA DE COMUNICACAO - SAIDA   DO MODULO GFCT5522 *
      *    I#GFCTG2 - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5523 *
      *    I#GFCTG3 - AREA DE COMUNICACAO - SAIDA   DO MODULO GFCT5523 *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS                      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    CBLTDLI  - OBTER E ENVIAR MENSAGENS PARA IMS                *
      *    POOL0025 - SOMAR/SUBTRAIR NUMERO DE DIAS DE UMA DATA        *
      *    POOL0081 - ALOCA DINAMICAMENTE UM MODULO                    *
      *    POOL1470 - CONSISTIR DATA                                   *
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
           '* AREAS PARA INDEXADORES *'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(005) COMP-3  VALUE ZEROS.
       77  IND-2                       PIC  9(005) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREAS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-NOME-TAB                PIC  X(011)         VALUE SPACES.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-ULTIMO-DIA              PIC  X(001)         VALUE SPACES.
       77  WRK-DATA-INCONST            PIC  X(001)         VALUE SPACES.
       77  WRK-MES-COMP                PIC  9(002)         VALUE ZEROS.
       77  WRK-DATA-PROCM              PIC  9(008)         VALUE ZEROS.
       77  WRK-DATA-INICIO             PIC  9(008)         VALUE ZEROS.
       77  WRK-DATA-FIM                PIC  9(008)         VALUE ZEROS.
       77  WRK-DATA-FIM-ANT            PIC  9(008)         VALUE ZEROS.
       77  WRK-ANOMES-CORRENTE         PIC  9(006)         VALUE ZEROS.
       77  WRK-ANOMES-CANCELA          PIC  9(006)         VALUE ZEROS.

       01  WRK-DATA-AUX.
           05  WRK-DIA-AUX             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-MES-AUX             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-ANO-AUX             PIC  9(004)         VALUE ZEROS.

       01  WRK-DATA-CAD.
           05  WRK-DIA-CAD             PIC  9(002)         VALUE ZEROS.
           05  WRK-MES-CAD             PIC  9(002)         VALUE ZEROS.
           05  WRK-ANO-CAD             PIC  9(004)         VALUE ZEROS.

       01  WRK-MSG01.
           05  FILLER                  PIC  X(026)         VALUE
               'ERRO NA CHAMADA DO MODULO'.
           05  WRK-NOME-MOD            PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA POOL0025 ***'.
      *----------------------------------------------------------------*

       01  WRK-DATA-ENTRADA          PIC  9(008)           VALUE ZEROS.
       01  WRK-NUMERO-DIAS           PIC  9(004)           VALUE ZEROS.
       01  WRK-DATA-SAIDA            PIC  9(008)           VALUE ZEROS.

BI0810 01  WRK-CFUNC-BDSCO.
BI0810     05  FILLER                  PIC  9(002)         VALUE ZEROS.
BI0810     05  WRK-FUNC-BDSCO          PIC  X(007)         VALUE SPACES.
BI0810 01  WRK-CFUNC-BDSCO-R           REDEFINES WRK-CFUNC-BDSCO
BI0810                                 PIC  9(009).

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
           '* AREA DE DB2 *'.
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
               INCLUDE GFCTB001
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB002
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB003
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

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTLJ'.

       COPY 'I#GFCTLK'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE DIVISION              USING GFCTLJ-ENTRADA
                                             GFCTLK-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *ROTINA INICIAL DO PROGRAMA                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM VARYING IND-1       FROM 1 BY 1
                     UNTIL IND-1       GREATER GFCTLJ-QTDE-OCOR
                         PERFORM 2000-PROCESSAR
           END-PERFORM.

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
                                          GFCTLK-SAIDA.

           INITIALIZE GFCT0M-AREA-ERROS
                      GFCTLK-SAIDA.

           PERFORM 1100-VERIFICAR-SIST-DISPONIVEL.

           PERFORM 1200-INICIALIZAR-SAIDA.

           PERFORM 1300-CONSISTIR-DADOS.

           PERFORM 1400-DATA-ATUAL.

           MOVE  1                     TO IND-2.

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
           MOVE GFCTLJ-TRANSACAO               TO GFCTFZ-TRANSACAO.
           MOVE GFCTLJ-FUNCAO                  TO GFCTFZ-FUNCAO.
           MOVE GFCTLJ-FUNC-BDSCO              TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                            TO GFCTFZ-FIM.

           CALL 'POOL0081'                     USING WRK-MODULO
                                                     GFCTFZ-ENTRADA
                                                     GFCTG1-SAIDA
                                                     GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                     NOT EQUAL ZEROS
               MOVE 9                          TO GFCTLK-ERRO
               MOVE WRK-MODULO                 TO WRK-NOME-MOD
                                                  GFCT0M-PROGRAMA
               MOVE WRK-MSG01                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0133'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO                     EQUAL 9
               IF  GFCT0M-TIPO-ACESSO          NOT EQUAL 'DB2'
                   MOVE GFCTG1-DESC-MSG-ERRO   TO GFCT0M-TEXTO
                   MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               ELSE
                   MOVE GFCTG1-COD-SQL-ERRO    TO GFCTLK-COD-SQL-ERRO
               END-IF
               MOVE WRK-MODULO                 TO GFCT0M-PROGRAMA
               MOVE 'GFCT0133'                 TO GFCT0M-TRANSACAO
               MOVE GFCTG1-ERRO                TO GFCTLK-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE              EQUAL 1
               MOVE 9                          TO GFCTLK-ERRO
               MOVE 'SISTEMA INDISPONIVEL'     TO GFCT0M-TEXTO
               MOVE WRK-MODULO                 TO GFCT0M-PROGRAMA
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0133'                 TO GFCT0M-TRANSACAO
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

           MOVE 280                    TO GFCTLK-LL.
           MOVE ZEROS                  TO GFCTLK-ZZ.
           MOVE GFCTLJ-TRANSACAO       TO GFCTLK-TRANSACAO.
           MOVE GFCTLJ-FUNCAO          TO GFCTLK-FUNCAO.
           MOVE GFCTLJ-QTDE-OCOR       TO GFCTLK-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTLK-QTDE-TOT-REG.
           MOVE GFCTLJ-FUNC-BDSCO      TO GFCTLK-FUNC-BDSCO.
           MOVE GFCTLJ-AGPTO           TO GFCTLK-AGPTO.
           MOVE 'S'                    TO GFCTLK-FIM.

           MOVE ZEROS                  TO GFCTLK-COD-SQL-ERRO
                                          GFCTLK-COD-MSG-ERRO
                                          GFCTLK-ERRO.
           MOVE '0000'                 TO GFCTG2-COD-MSG.

           PERFORM 1320-OBTER-DESC-MENSAGEM.

           MOVE GFCTG3-DESC-MSG        TO GFCTLK-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR DADOS                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF  GFCTLJ-TRANSACAO            EQUAL LOW-VALUES          OR
               GFCTLJ-TRANSACAO            EQUAL SPACES              OR
               GFCTLJ-FUNCAO               EQUAL LOW-VALUES          OR
               GFCTLJ-FUNCAO               EQUAL SPACES              OR
               GFCTLJ-FUNC-BDSCO           EQUAL LOW-VALUES          OR
               GFCTLJ-FUNC-BDSCO           EQUAL SPACES              OR
               GFCTLJ-QTDE-OCOR            NOT NUMERIC               OR
               GFCTLJ-QTDE-OCOR            EQUAL ZEROS               OR
               GFCTLJ-QTDE-TOT-REG         NOT NUMERIC               OR
              (GFCTLJ-FIM                  NOT EQUAL 'S' AND 'N')    OR
              (GFCTLJ-AGPTO                NOT EQUAL 004 AND 009 AND
                                                     014 AND 015 AND
                                                     016 AND 017 AND
                                                     018 AND 019 AND
                                                     020)
               MOVE 1                      TO GFCTLK-ERRO
                                              GFCTLK-COD-MSG-ERRO
               MOVE ZEROS                  TO GFCTLK-COD-SQL-ERRO
               MOVE '0001'                 TO GFCTG2-COD-MSG

               PERFORM 1310-MONTAR-AREA-SAIDA

               PERFORM 1320-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG        TO GFCTLK-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1310-MONTAR-AREA-SAIDA          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLJ-QTDE-TOT-REG    TO GFCTLK-QTDE-TOT-REG.

           COMPUTE GFCTLK-LL            = GFCTLK-LL
                                          + (GFCTLK-QTDE-OCOR * 146).

           PERFORM
           VARYING IND-1               FROM 1 BY 1
             UNTIL IND-1               GREATER GFCTLK-QTDE-OCOR
                   MOVE GFCTLJ-SEQUENCIA      (IND-1)
                                       TO GFCTLK-SEQUENCIA      (IND-1)
                   MOVE GFCTLJ-PACOTE         (IND-1)
                                       TO GFCTLK-PACOTE         (IND-1)
                   MOVE GFCTLJ-DESC-PACOTE    (IND-1)
                                       TO GFCTLK-DESC-PACOTE    (IND-1)
                   MOVE GFCTLJ-CPF-CNPJ       (IND-1)
                                       TO GFCTLK-CPF-CNPJ       (IND-1)
                   MOVE GFCTLJ-FILIAL         (IND-1)
                                       TO GFCTLK-FILIAL         (IND-1)
                   MOVE GFCTLJ-CONTROLE       (IND-1)
                                       TO GFCTLK-CONTROLE       (IND-1)
                   MOVE GFCTLJ-AGENCIA        (IND-1)
                                       TO GFCTLK-AGENCIA        (IND-1)
                   MOVE GFCTLJ-CONTA          (IND-1)
                                       TO GFCTLK-CONTA          (IND-1)
                   MOVE GFCTLJ-POSTO          (IND-1)
                                       TO GFCTLK-POSTO          (IND-1)
                   MOVE GFCTLJ-SEGMENTO       (IND-1)
                                       TO GFCTLK-SEGMENTO       (IND-1)
                   MOVE GFCTLJ-MUNICIPIO      (IND-1)
                                       TO GFCTLK-MUNICIPIO      (IND-1)
                   MOVE GFCTLJ-UF             (IND-1)
                                       TO GFCTLK-UF             (IND-1)
                   MOVE GFCTLJ-DESC-NOME      (IND-1)
                                       TO GFCTLK-DESC-NOME      (IND-1)
                   MOVE GFCTLJ-DATA-INICIO-ANT(IND-1)
                                       TO GFCTLK-DATA-INICIO-ANT
                                                                (IND-1)
                   MOVE GFCTLJ-DATA-FIM-ANT   (IND-1)
                                       TO GFCTLK-DATA-FIM-ANT   (IND-1)
                   MOVE GFCTLJ-DATA-INICIO    (IND-1)
                                       TO GFCTLK-DATA-INICIO    (IND-1)
                   MOVE GFCTLJ-DATA-FIM       (IND-1)
                                       TO GFCTLK-DATA-FIM       (IND-1)
                   MOVE ZEROS          TO GFCTLK-ERRO-OCOR      (IND-1)
           END-PERFORM.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA OBTER DESCRICAO MENSAGEM                            *
      ******************************************************************
      *----------------------------------------------------------------*
       1320-OBTER-DESC-MENSAGEM        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                   TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE 100                      TO GFCTG2-LL.
           MOVE ZEROS                    TO GFCTG2-ZZ.
           MOVE GFCTLJ-TRANSACAO         TO GFCTG2-TRANSACAO.
           MOVE GFCTLJ-FUNCAO            TO GFCTG2-FUNCAO.
           MOVE GFCTLJ-FUNC-BDSCO        TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                      TO GFCTG2-TIPO-PROC.
           MOVE 'N'                      TO GFCTG2-FIM.
           MOVE 'GFCT5523'               TO WRK-MODULO.

           CALL 'POOL0081'               USING WRK-MODULO
                                               GFCTG2-ENTRADA
                                               GFCTG3-SAIDA
                                               GFCT0M-AREA-ERROS.

           IF  RETURN-CODE               NOT EQUAL ZEROS OR
               GFCTG3-ERRO               EQUAL 9
               MOVE 9                    TO GFCTLK-ERRO
               MOVE WRK-MODULO           TO GFCT0M-PROGRAMA
                                            WRK-NOME-MOD
               MOVE WRK-MSG01            TO GFCT0M-TEXTO
               MOVE 'APL'                TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0133'           TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO               EQUAL 1
               MOVE 1                    TO GFCTLK-ERRO
               MOVE GFCTG3-COD-SQL-ERRO  TO GFCTLK-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO  TO GFCTLK-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO TO GFCTLK-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA DATA ATUAL                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1400-DATA-ATUAL                 SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ANTER,
                   DPROCM_ATUAL,
                   DPROCM_PROX
             INTO  :GFCTB0A1.DPROCM-ANTER,
                   :GFCTB0A1.DPROCM-ATUAL,
                   :GFCTB0A1.DPROCM-PROX
             FROM  DB2PRD.PARM_DATA_PROCM
             WHERE CSIST_PRINC         = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1320-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0A1'      TO WRK-NOME-TAB
               MOVE 'GFCT0133'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTLK-ERRO
               MOVE  SQLCODE           TO GFCTLK-COD-SQL-ERRO
               MOVE  SPACES            TO GFCTLK-DESC-MSG-ERRO
               MOVE  0012              TO GFCTLK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE  DPROCM-ATUAL          OF GFCTB0A1(7:4)
                                       TO WRK-DATA-PROCM(1:4).
           MOVE  DPROCM-ATUAL          OF GFCTB0A1(4:2)
                                       TO WRK-DATA-PROCM(5:2).
           MOVE  DPROCM-ATUAL          OF GFCTB0A1(1:2)
                                       TO WRK-DATA-PROCM(7:2).

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONCATENAR MENSAGEM                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1410-CONCATENAR-MENSAGEM        SECTION.
      *----------------------------------------------------------------*

           STRING GFCTG3-DESC-MSG      WRK-NOME-TAB

           DELIMITED BY '  '           INTO GFCTLK-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PRINCIPAL DO PROGRAMA                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-MONTAR-DATA-INVERTIDA.

           INSPECT GFCTLJ-DATA-INICIO(IND-1) REPLACING ALL '/' BY '.'.
           INSPECT GFCTLJ-DATA-FIM(IND-1)    REPLACING ALL '/' BY '.'.

           PERFORM 2200-AVALIAR-OCORRENCIAS.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA INVERTER DATAS DO INC I#GFCTLJ.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-MONTAR-DATA-INVERTIDA      SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLJ-DATA-INICIO(IND-1)(7:4) TO WRK-DATA-INICIO(1:4).
           MOVE GFCTLJ-DATA-INICIO(IND-1)(4:2) TO WRK-DATA-INICIO(5:2).
           MOVE GFCTLJ-DATA-INICIO(IND-1)(1:2) TO WRK-DATA-INICIO(7:2).

           MOVE GFCTLJ-DATA-FIM(IND-1)(7:4)    TO WRK-DATA-FIM(1:4).
           MOVE GFCTLJ-DATA-FIM(IND-1)(4:2)    TO WRK-DATA-FIM(5:2).
           MOVE GFCTLJ-DATA-FIM(IND-1)(1:2)    TO WRK-DATA-FIM(7:2).

           MOVE GFCTLJ-DATA-FIM-ANT(IND-1)(7:4)
                                               TO WRK-DATA-FIM-ANT(1:4).
           MOVE GFCTLJ-DATA-FIM-ANT(IND-1)(4:2)
                                               TO WRK-DATA-FIM-ANT(5:2).
           MOVE GFCTLJ-DATA-FIM-ANT(IND-1)(1:2)
                                               TO WRK-DATA-FIM-ANT(7:2).

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA VERIFICAR INCONSISTENCIAS NA DATA.                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-AVALIAR-OCORRENCIAS        SECTION.
      *----------------------------------------------------------------*

           IF  GFCTLJ-DATA-FIM(IND-1)          EQUAL SPACES
               IF GFCTLK-ERRO EQUAL ZEROS
                  MOVE ZEROS                   TO GFCTLK-COD-SQL-ERRO
                  MOVE  0766                   TO GFCTLK-COD-MSG-ERRO
                  MOVE '0766'                  TO GFCTG2-COD-MSG
                  PERFORM 1320-OBTER-DESC-MENSAGEM
                  MOVE GFCTG3-DESC-MSG         TO GFCTLK-DESC-MSG-ERRO
               END-IF
               PERFORM 2210-MOVIMENTAR-DADOS
               GO TO 2200-99-FIM
           END-IF.

           MOVE  GFCTLJ-DATA-FIM(IND-1)    TO WRK-DATA-AUX.
           MOVE  WRK-DIA-AUX               TO WRK-DIA-CAD.
           MOVE  WRK-MES-AUX               TO WRK-MES-CAD.
           MOVE  WRK-ANO-AUX               TO WRK-ANO-CAD.

           CALL 'POOL1470'                 USING WRK-DATA-CAD.

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               IF GFCTLK-ERRO EQUAL ZEROS
                  MOVE ZEROS               TO GFCTLK-COD-SQL-ERRO
                  MOVE  0767               TO GFCTLK-COD-MSG-ERRO
                  MOVE '0767'              TO GFCTG2-COD-MSG
                  PERFORM 1320-OBTER-DESC-MENSAGEM
                  MOVE GFCTG3-DESC-MSG     TO GFCTLK-DESC-MSG-ERRO
               END-IF
               PERFORM 2210-MOVIMENTAR-DADOS
               GO TO 2200-99-FIM
           END-IF.

           IF  WRK-DATA-FIM                EQUAL WRK-DATA-INICIO
BI0810         IF  WRK-DATA-FIM            GREATER WRK-DATA-PROCM
                   GO TO 0000-SO-CANCELA-FISICO-FUTURO
               END-IF
           END-IF.

           PERFORM 2240-VERIFICAR-ULTIMO-DIA.

           IF  WRK-DATA-INCONST            EQUAL 'S'
               GO TO 2200-99-FIM
           ELSE
               IF  WRK-ULTIMO-DIA          EQUAL 'N'
                  IF GFCTLK-ERRO EQUAL ZEROS
                     MOVE ZEROS            TO GFCTLK-COD-SQL-ERRO
                     MOVE  0768            TO GFCTLK-COD-MSG-ERRO
                     MOVE '0768'           TO GFCTG2-COD-MSG
                     PERFORM 1320-OBTER-DESC-MENSAGEM
                     MOVE GFCTG3-DESC-MSG  TO GFCTLK-DESC-MSG-ERRO
                  END-IF
                  PERFORM 2210-MOVIMENTAR-DADOS
                  GO TO 2200-99-FIM
               END-IF
           END-IF.

           IF  WRK-DATA-FIM                NOT LESS WRK-DATA-FIM-ANT
               IF GFCTLK-ERRO EQUAL ZEROS
                  MOVE ZEROS               TO GFCTLK-COD-SQL-ERRO
                  MOVE  0769               TO GFCTLK-COD-MSG-ERRO
                  MOVE '0769'              TO GFCTG2-COD-MSG
                  PERFORM 1320-OBTER-DESC-MENSAGEM
                  MOVE GFCTG3-DESC-MSG     TO GFCTLK-DESC-MSG-ERRO
               END-IF
               PERFORM 2210-MOVIMENTAR-DADOS
               GO TO 2200-99-FIM
           END-IF.

           IF  WRK-DATA-FIM                LESS WRK-DATA-INICIO
               IF GFCTLK-ERRO EQUAL ZEROS
                  MOVE ZEROS               TO GFCTLK-COD-SQL-ERRO
                  MOVE  0770               TO GFCTLK-COD-MSG-ERRO
                  MOVE '0770'              TO GFCTG2-COD-MSG
                  PERFORM 1320-OBTER-DESC-MENSAGEM
                  MOVE GFCTG3-DESC-MSG     TO GFCTLK-DESC-MSG-ERRO
               END-IF
               PERFORM 2210-MOVIMENTAR-DADOS
               GO TO 2200-99-FIM
           END-IF.

BI0810     IF  WRK-DATA-FIM                NOT GREATER WRK-DATA-PROCM
               IF GFCTLK-ERRO EQUAL ZEROS
                  MOVE ZEROS               TO GFCTLK-COD-SQL-ERRO
                  MOVE  0771               TO GFCTLK-COD-MSG-ERRO
                  MOVE '0771'              TO GFCTG2-COD-MSG
                  PERFORM 1320-OBTER-DESC-MENSAGEM
                  MOVE GFCTG3-DESC-MSG     TO GFCTLK-DESC-MSG-ERRO
               END-IF
               PERFORM 2210-MOVIMENTAR-DADOS
               GO TO 2200-99-FIM
           END-IF.

      *----------------------------------------------------------------*
       0000-SO-CANCELA-FISICO-FUTURO.
      *----------------------------------------------------------------*

           IF  WRK-DATA-FIM                  EQUAL WRK-DATA-INICIO
               PERFORM 2220-EXCLUSAO-FISICA
           ELSE
               PERFORM 2230-EXCLUSAO-LOGICA
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA MOVIMENTAR DADOS.                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-MOVIMENTAR-DADOS           SECTION.
      *----------------------------------------------------------------*

           ADD   1                         TO GFCTLK-QTDE-TOT-REG.

           COMPUTE GFCTLK-LL                = GFCTLK-LL + 146.

           MOVE  1                         TO GFCTLK-ERRO
                                              GFCTLK-ERRO-OCOR(IND-2).
           MOVE GFCTLJ-SEQUENCIA(IND-1)    TO GFCTLK-SEQUENCIA(IND-2).
           MOVE GFCTLJ-PACOTE(IND-1)       TO GFCTLK-PACOTE(IND-2).
           MOVE GFCTLJ-DESC-PACOTE(IND-1)  TO GFCTLK-DESC-PACOTE(IND-2).
           MOVE GFCTLJ-CPF-CNPJ(IND-1)     TO GFCTLK-CPF-CNPJ(IND-2).
           MOVE GFCTLJ-FILIAL(IND-1)       TO GFCTLK-FILIAL(IND-2).
           MOVE GFCTLJ-CONTROLE(IND-1)     TO GFCTLK-CONTROLE(IND-2).
           MOVE GFCTLJ-AGENCIA(IND-1)      TO GFCTLK-AGENCIA(IND-2).
           MOVE GFCTLJ-CONTA(IND-1)        TO GFCTLK-CONTA(IND-2).
           MOVE GFCTLJ-POSTO(IND-1)        TO GFCTLK-POSTO(IND-2).
           MOVE GFCTLJ-SEGMENTO(IND-1)     TO GFCTLK-SEGMENTO(IND-2).
           MOVE GFCTLJ-MUNICIPIO(IND-1)    TO GFCTLK-MUNICIPIO(IND-2).
           MOVE GFCTLJ-UF(IND-1)           TO GFCTLK-UF(IND-2).
           MOVE GFCTLJ-DESC-NOME(IND-1)    TO GFCTLK-DESC-NOME(IND-2).
           MOVE GFCTLJ-DATA-INICIO-ANT(IND-1)
                                       TO GFCTLK-DATA-INICIO-ANT(IND-2).
           MOVE GFCTLJ-DATA-FIM-ANT(IND-1)
                                       TO GFCTLK-DATA-FIM-ANT(IND-2).
           MOVE GFCTLJ-DATA-INICIO(IND-1)  TO GFCTLK-DATA-INICIO(IND-2).
           MOVE GFCTLJ-DATA-FIM(IND-1)     TO GFCTLK-DATA-FIM(IND-2).

           ADD 1                           TO IND-2.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA EXCLUIR FISICAMENTE.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2220-EXCLUSAO-FISICA            SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTLJ-AGPTO
               WHEN 15
                   PERFORM 2221-EXCLUIR-PAB

               WHEN 14
                   PERFORM 2222-EXCLUIR-AGENCIA

               WHEN 16
                   PERFORM 2223-EXCLUIR-EMPRESA

               WHEN 17
                   PERFORM 2224-EXCLUIR-POSTAL-EXP

               WHEN 20
                   PERFORM 2224-EXCLUIR-POSTAL-EXP

               WHEN 04
                   PERFORM 2225-EXCLUIR-CLIENTE

               WHEN 09
                   PERFORM 2226-EXCLUIR-SEGMENTO

               WHEN 18
                   PERFORM 2227-EXCLUIR-MUNICIPIO

               WHEN 19
                   PERFORM 2228-EXCLUIR-UF

           END-EVALUATE.

           PERFORM 2229-EXCLUIR-PRINCIPAL.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA EXCLUIR PAB.                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2221-EXCLUIR-PAB                SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLJ-PACOTE(IND-1)    TO CSERVC-TARIF     OF GFCTB005.
           MOVE GFCTLJ-AGPTO            TO CAGPTO-CTA       OF GFCTB005.
           MOVE GFCTLJ-DATA-INICIO(IND-1)
                                        TO DINIC-ADSAO-COMP OF GFCTB005.
           MOVE GFCTLJ-SEQUENCIA(IND-1) TO CSEQ-AGPTO-CTA   OF GFCTB005.

           EXEC SQL
               DELETE FROM DB2PRD.ADSAO_GRP_PAB
               WHERE
                   CSERVC_TARIF        = :GFCTB005.CSERVC-TARIF     AND
                   CAGPTO_CTA          = :GFCTB005.CAGPTO-CTA       AND
                   DINIC_ADSAO_COMP    = :GFCTB005.DINIC-ADSAO-COMP AND
                   CSEQ_AGPTO_CTA      = :GFCTB005.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE '0020'             TO GFCTG2-COD-MSG
               PERFORM 1320-OBTER-DESC-MENSAGEM
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE ' - GFCTB005'      TO WRK-NOME-TAB
               MOVE 'GFCT0133'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'ADSAO_GRP_PAB'    TO GFCT0M-NOME-TAB
               MOVE 'DELETE'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTLK-ERRO
               MOVE  SQLCODE           TO GFCTLK-COD-SQL-ERRO
               MOVE  SPACES            TO GFCTLK-DESC-MSG-ERRO
               MOVE  0020              TO GFCTLK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2221-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA EXCLUIR AGENCIA.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2222-EXCLUIR-AGENCIA            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLJ-PACOTE(IND-1)    TO CSERVC-TARIF     OF GFCTB002.
           MOVE GFCTLJ-AGPTO            TO CAGPTO-CTA       OF GFCTB002.
           MOVE GFCTLJ-DATA-INICIO(IND-1)
                                        TO DINIC-ADSAO-COMP OF GFCTB002.
           MOVE GFCTLJ-SEQUENCIA(IND-1) TO CSEQ-AGPTO-CTA   OF GFCTB002.

           EXEC SQL
               DELETE FROM DB2PRD.ADSAO_GRP_DEPDC
               WHERE
                   CSERVC_TARIF        = :GFCTB002.CSERVC-TARIF     AND
                   CAGPTO_CTA          = :GFCTB002.CAGPTO-CTA       AND
                   DINIC_ADSAO_COMP    = :GFCTB002.DINIC-ADSAO-COMP AND
                   CSEQ_AGPTO_CTA      = :GFCTB002.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE '0020'             TO GFCTG2-COD-MSG
               PERFORM 1320-OBTER-DESC-MENSAGEM
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE ' - GFCTB002'      TO WRK-NOME-TAB
               MOVE 'GFCT0133'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'ADSAO_GRP_DEPDC'  TO GFCT0M-NOME-TAB
               MOVE 'DELETE'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTLK-ERRO
               MOVE  SQLCODE           TO GFCTLK-COD-SQL-ERRO
               MOVE  SPACES            TO GFCTLK-DESC-MSG-ERRO
               MOVE  0020              TO GFCTLK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2222-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA EXCLUIR EMPRESA.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2223-EXCLUIR-EMPRESA            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLJ-PACOTE(IND-1)    TO CSERVC-TARIF     OF GFCTB003.
           MOVE GFCTLJ-AGPTO            TO CAGPTO-CTA       OF GFCTB003.
           MOVE GFCTLJ-DATA-INICIO(IND-1)
                                        TO DINIC-ADSAO-COMP OF GFCTB003.
           MOVE GFCTLJ-SEQUENCIA(IND-1) TO CSEQ-AGPTO-CTA   OF GFCTB003.

           EXEC SQL
               DELETE FROM DB2PRD.ADSAO_GRP_EMPR
               WHERE
                   CSERVC_TARIF        = :GFCTB003.CSERVC-TARIF     AND
                   CAGPTO_CTA          = :GFCTB003.CAGPTO-CTA       AND
                   DINIC_ADSAO_COMP    = :GFCTB003.DINIC-ADSAO-COMP AND
                   CSEQ_AGPTO_CTA      = :GFCTB003.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE '0020'             TO GFCTG2-COD-MSG
               PERFORM 1320-OBTER-DESC-MENSAGEM
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE ' - GFCTB003'      TO WRK-NOME-TAB
               MOVE 'GFCT0133'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'ADSAO_GRP_EMPR'   TO GFCT0M-NOME-TAB
               MOVE 'DELETE'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTLK-ERRO
               MOVE  SQLCODE           TO GFCTLK-COD-SQL-ERRO
               MOVE  SPACES            TO GFCTLK-DESC-MSG-ERRO
               MOVE  0020              TO GFCTLK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2223-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA EXCLUIR POSTAL.                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2224-EXCLUIR-POSTAL-EXP         SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLJ-PACOTE(IND-1)    TO CSERVC-TARIF     OF GFCTB006.
           MOVE GFCTLJ-AGPTO            TO CAGPTO-CTA       OF GFCTB006.
           MOVE GFCTLJ-DATA-INICIO(IND-1)
                                        TO DINIC-ADSAO-COMP OF GFCTB006.
           MOVE GFCTLJ-SEQUENCIA(IND-1) TO CSEQ-AGPTO-CTA   OF GFCTB006.

           EXEC SQL
               DELETE FROM DB2PRD.ADSAO_GRP_PSTAL
               WHERE
                   CSERVC_TARIF        = :GFCTB006.CSERVC-TARIF     AND
                   CAGPTO_CTA          = :GFCTB006.CAGPTO-CTA       AND
                   DINIC_ADSAO_COMP    = :GFCTB006.DINIC-ADSAO-COMP AND
                   CSEQ_AGPTO_CTA      = :GFCTB006.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE '0020'             TO GFCTG2-COD-MSG
               PERFORM 1320-OBTER-DESC-MENSAGEM
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE ' - GFCTB006'      TO WRK-NOME-TAB
               MOVE 'GFCT0133'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'ADSAO_GRP_PSTAL'  TO GFCT0M-NOME-TAB
               MOVE 'DELETE'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTLK-ERRO
               MOVE  SQLCODE           TO GFCTLK-COD-SQL-ERRO
               MOVE  SPACES            TO GFCTLK-DESC-MSG-ERRO
               MOVE  0020              TO GFCTLK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2224-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA EXCLUIR CLIENTE.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2225-EXCLUIR-CLIENTE            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLJ-PACOTE(IND-1)    TO CSERVC-TARIF     OF GFCTB0H9.
           MOVE GFCTLJ-AGPTO            TO CAGPTO-CTA       OF GFCTB0H9.
           MOVE GFCTLJ-DATA-INICIO(IND-1)
                                        TO DINIC-ADSAO-COMP OF GFCTB0H9.
           MOVE GFCTLJ-SEQUENCIA(IND-1) TO CSEQ-AGPTO-CTA   OF GFCTB0H9.

           EXEC SQL
               DELETE FROM DB2PRD.TADSAO_GRP_CLI
               WHERE
                   CSERVC_TARIF        = :GFCTB0H9.CSERVC-TARIF     AND
                   CAGPTO_CTA          = :GFCTB0H9.CAGPTO-CTA       AND
                   DINIC_ADSAO_COMP    = :GFCTB0H9.DINIC-ADSAO-COMP AND
                   CSEQ_AGPTO_CTA      = :GFCTB0H9.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE '0020'             TO GFCTG2-COD-MSG
               PERFORM 1320-OBTER-DESC-MENSAGEM
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE ' - GFCTB0H9'      TO WRK-NOME-TAB
               MOVE 'GFCT0133'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TADSAO_GRP_CLI '  TO GFCT0M-NOME-TAB
               MOVE 'DELETE'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTLK-ERRO
               MOVE  SQLCODE           TO GFCTLK-COD-SQL-ERRO
               MOVE  SPACES            TO GFCTLK-DESC-MSG-ERRO
               MOVE  0020              TO GFCTLK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
      *2225-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA EXCLUIR SEGMENTO.                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2226-EXCLUIR-SEGMENTO           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLJ-PACOTE(IND-1)    TO CSERVC-TARIF     OF GFCTB007.
           MOVE GFCTLJ-AGPTO            TO CAGPTO-CTA       OF GFCTB007.
           MOVE GFCTLJ-DATA-INICIO(IND-1)
                                        TO DINIC-ADSAO-COMP OF GFCTB007.
           MOVE GFCTLJ-SEQUENCIA(IND-1) TO CSEQ-AGPTO-CTA   OF GFCTB007.

           EXEC SQL
               DELETE FROM DB2PRD.ADSAO_GRP_SGMTO
               WHERE
                   CSERVC_TARIF        = :GFCTB007.CSERVC-TARIF     AND
                   CAGPTO_CTA          = :GFCTB007.CAGPTO-CTA       AND
                   DINIC_ADSAO_COMP    = :GFCTB007.DINIC-ADSAO-COMP AND
                   CSEQ_AGPTO_CTA      = :GFCTB007.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE '0020'             TO GFCTG2-COD-MSG
               PERFORM 1320-OBTER-DESC-MENSAGEM
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE ' - GFCTB007'      TO WRK-NOME-TAB
               MOVE 'GFCT0133'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'ADSAO_GRP_SGMTO'  TO GFCT0M-NOME-TAB
               MOVE 'DELETE'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTLK-ERRO
               MOVE  SQLCODE           TO GFCTLK-COD-SQL-ERRO
               MOVE  SPACES            TO GFCTLK-DESC-MSG-ERRO
               MOVE  0020              TO GFCTLK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2226-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA EXCLUIR MUNICIPIO.                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2227-EXCLUIR-MUNICIPIO          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLJ-PACOTE(IND-1)    TO CSERVC-TARIF     OF GFCTB004.
           MOVE GFCTLJ-AGPTO            TO CAGPTO-CTA       OF GFCTB004.
           MOVE GFCTLJ-DATA-INICIO(IND-1)
                                        TO DINIC-ADSAO-COMP OF GFCTB004.
           MOVE GFCTLJ-SEQUENCIA(IND-1) TO CSEQ-AGPTO-CTA   OF GFCTB004.

           EXEC SQL
               DELETE FROM DB2PRD.ADSAO_GRP_MUN
               WHERE
                   CSERVC_TARIF        = :GFCTB004.CSERVC-TARIF     AND
                   CAGPTO_CTA          = :GFCTB004.CAGPTO-CTA       AND
                   DINIC_ADSAO_COMP    = :GFCTB004.DINIC-ADSAO-COMP AND
                   CSEQ_AGPTO_CTA      = :GFCTB004.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE '0020'             TO GFCTG2-COD-MSG
               PERFORM 1320-OBTER-DESC-MENSAGEM
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE ' - GFCTB004'      TO WRK-NOME-TAB
               MOVE 'GFCT0133'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'ADSAO_GRP_MUN'    TO GFCT0M-NOME-TAB
               MOVE 'DELETE'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTLK-ERRO
               MOVE  SQLCODE           TO GFCTLK-COD-SQL-ERRO
               MOVE  SPACES            TO GFCTLK-DESC-MSG-ERRO
               MOVE  0020              TO GFCTLK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2227-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA EXCLUIR UF.                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2228-EXCLUIR-UF                 SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLJ-PACOTE(IND-1)    TO CSERVC-TARIF     OF GFCTB008.
           MOVE GFCTLJ-AGPTO            TO CAGPTO-CTA       OF GFCTB008.
           MOVE GFCTLJ-DATA-INICIO(IND-1)
                                        TO DINIC-ADSAO-COMP OF GFCTB008.
           MOVE GFCTLJ-SEQUENCIA(IND-1) TO CSEQ-AGPTO-CTA   OF GFCTB008.

           EXEC SQL
               DELETE FROM DB2PRD.ADSAO_GRP_UF
               WHERE
                   CSERVC_TARIF        = :GFCTB008.CSERVC-TARIF     AND
                   CAGPTO_CTA          = :GFCTB008.CAGPTO-CTA       AND
                   DINIC_ADSAO_COMP    = :GFCTB008.DINIC-ADSAO-COMP AND
                   CSEQ_AGPTO_CTA      = :GFCTB008.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE '0020'             TO GFCTG2-COD-MSG
               PERFORM 1320-OBTER-DESC-MENSAGEM
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE ' - GFCTB008'      TO WRK-NOME-TAB
               MOVE 'GFCT0133'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'ADSAO_GRP_UF'     TO GFCT0M-NOME-TAB
               MOVE 'DELETE'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0090'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTLK-ERRO
               MOVE  SQLCODE           TO GFCTLK-COD-SQL-ERRO
               MOVE  SPACES            TO GFCTLK-DESC-MSG-ERRO
               MOVE  0020              TO GFCTLK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2228-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA EXCLUIR PRINCIPAL.                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2229-EXCLUIR-PRINCIPAL          SECTION.
      *----------------------------------------------------------------*

           PERFORM   2229-1-JOGA-EXCECAO.

           MOVE GFCTLJ-PACOTE(IND-1)    TO CSERVC-TARIF     OF GFCTB001.
           MOVE GFCTLJ-AGPTO            TO CAGPTO-CTA       OF GFCTB001.
           MOVE GFCTLJ-DATA-INICIO(IND-1)
                                        TO DINIC-ADSAO-COMP OF GFCTB001.
           MOVE GFCTLJ-SEQUENCIA(IND-1) TO CSEQ-AGPTO-CTA   OF GFCTB001.

           EXEC SQL
               DELETE FROM DB2PRD.ADSAO_COMP_PCOTE
               WHERE
                   CSERVC_TARIF        = :GFCTB001.CSERVC-TARIF     AND
                   CAGPTO_CTA          = :GFCTB001.CAGPTO-CTA       AND
                   DINIC_ADSAO_COMP    = :GFCTB001.DINIC-ADSAO-COMP AND
                   CSEQ_AGPTO_CTA      = :GFCTB001.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE '0020'             TO GFCTG2-COD-MSG
               PERFORM 1320-OBTER-DESC-MENSAGEM
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE ' - GFCTB001'      TO WRK-NOME-TAB
               MOVE 'GFCT0133'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'ADSAO_COMP_PCOTE' TO GFCT0M-NOME-TAB
               MOVE 'DELETE'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0100'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTLK-ERRO
               MOVE  SQLCODE           TO GFCTLK-COD-SQL-ERRO
               MOVE  SPACES            TO GFCTLK-DESC-MSG-ERRO
               MOVE  0020              TO GFCTLK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2229-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA JOGAR FORA AS EXCESSOES - TODAS FUTURAS!            *
      ******************************************************************
      *----------------------------------------------------------------*
       2229-1-JOGA-EXCECAO             SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLJ-PACOTE(IND-1)    TO CSERVC-TARIF     OF GFCTB048.
           MOVE GFCTLJ-AGPTO            TO CAGPTO-CTA       OF GFCTB048.
           MOVE GFCTLJ-DATA-INICIO(IND-1)
                                        TO DINIC-ADSAO-COMP OF GFCTB048.
           MOVE GFCTLJ-SEQUENCIA(IND-1) TO CSEQ-AGPTO-CTA   OF GFCTB048.

           EXEC SQL
               DELETE FROM DB2PRD.EXCEC_ADSAO_COMP
               WHERE
                   CSERVC_TARIF        = :GFCTB048.CSERVC-TARIF     AND
                   CAGPTO_CTA          = :GFCTB048.CAGPTO-CTA       AND
                   DINIC_ADSAO_COMP    = :GFCTB048.DINIC-ADSAO-COMP AND
                   CSEQ_AGPTO_CTA      = :GFCTB048.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE '0020'             TO GFCTG2-COD-MSG
               PERFORM 1320-OBTER-DESC-MENSAGEM
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE ' - GFCTB048'      TO WRK-NOME-TAB
               MOVE 'GFCT0133'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'EXCEC_ADSAO_COMP' TO GFCT0M-NOME-TAB
               MOVE 'DELETE'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0110'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTLK-ERRO
               MOVE  SQLCODE           TO GFCTLK-COD-SQL-ERRO
               MOVE  SPACES            TO GFCTLK-DESC-MSG-ERRO
               MOVE  0020              TO GFCTLK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2229-1-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA EXCLUIR LOGICAMENTE.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2230-EXCLUSAO-LOGICA            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLJ-DATA-FIM(IND-1)  TO DFIM-ADSAO-COMP  OF GFCTB001.
           MOVE GFCTLJ-PACOTE(IND-1)    TO CSERVC-TARIF     OF GFCTB001.
           MOVE GFCTLJ-AGPTO            TO CAGPTO-CTA       OF GFCTB001.
           MOVE GFCTLJ-DATA-INICIO(IND-1)
                                        TO DINIC-ADSAO-COMP OF GFCTB001.
           MOVE GFCTLJ-SEQUENCIA(IND-1) TO CSEQ-AGPTO-CTA   OF GFCTB001.
BI0810     MOVE GFCTLJ-FUNC-BDSCO       TO WRK-FUNC-BDSCO.
BI0810     MOVE WRK-CFUNC-BDSCO-R       TO CFUNC-MANUT-EXCL OF GFCTB001.

           EXEC SQL
               UPDATE DB2PRD.ADSAO_COMP_PCOTE
               SET DFIM_ADSAO_COMP     = :GFCTB001.DFIM-ADSAO-COMP,
BI0810             CFUNC_MANUT_EXCL    = :GFCTB001.CFUNC-MANUT-EXCL,
BI0810             HMANUT_REG          = CURRENT TIMESTAMP
               WHERE
                   CSERVC_TARIF        = :GFCTB001.CSERVC-TARIF     AND
                   CAGPTO_CTA          = :GFCTB001.CAGPTO-CTA       AND
                   DINIC_ADSAO_COMP    = :GFCTB001.DINIC-ADSAO-COMP AND
                   CSEQ_AGPTO_CTA      = :GFCTB001.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE '0019'             TO GFCTG2-COD-MSG
               PERFORM 1320-OBTER-DESC-MENSAGEM
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE ' - GFCTB001'      TO WRK-NOME-TAB
               MOVE 'GFCT0133'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'ADSAO_COMP_PCOTE' TO GFCT0M-NOME-TAB
               MOVE 'UPDATE'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0120'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTLK-ERRO
               MOVE  SQLCODE           TO GFCTLK-COD-SQL-ERRO
               MOVE  SPACES            TO GFCTLK-DESC-MSG-ERRO
               MOVE  0019              TO GFCTLK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA VERIFICAR SE O DIA DO CAMPO GFCTLJ-DATA-FIM E O UL- *
      *   TIMO DIA DO MES.                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2240-VERIFICAR-ULTIMO-DIA       SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                            TO WRK-ULTIMO-DIA
                                                  WRK-DATA-INCONST.
           MOVE  GFCTLJ-DATA-FIM(IND-1)(1:2)   TO WRK-DATA-ENTRADA(7:2).
           MOVE  GFCTLJ-DATA-FIM(IND-1)(4:2)   TO WRK-DATA-ENTRADA(5:2)
                                                  WRK-MES-COMP.
           MOVE  GFCTLJ-DATA-FIM(IND-1)(7:4)   TO WRK-DATA-ENTRADA(1:4).
           MOVE  1                             TO WRK-NUMERO-DIAS.

           CALL 'POOL0025'                     USING WRK-DATA-ENTRADA
                                                     WRK-NUMERO-DIAS
                                                    WRK-DATA-SAIDA.

           IF  RETURN-CODE                     NOT EQUAL ZEROS
               IF GFCTLK-ERRO EQUAL ZEROS
                  MOVE ZEROS                   TO GFCTLK-COD-SQL-ERRO
                  MOVE  0767                   TO GFCTLK-COD-MSG-ERRO
                  MOVE '0767'                  TO GFCTG2-COD-MSG
                  PERFORM 1320-OBTER-DESC-MENSAGEM
                  MOVE GFCTG3-DESC-MSG         TO GFCTLK-DESC-MSG-ERRO
               END-IF
               PERFORM 2210-MOVIMENTAR-DADOS
               MOVE 'S'                        TO WRK-DATA-INCONST
           END-IF.

           IF  WRK-DATA-SAIDA(5:2)             NOT EQUAL WRK-MES-COMP
               MOVE 'S'                        TO WRK-ULTIMO-DIA
           END-IF.

      *----------------------------------------------------------------*
       2240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA FINALIZAR PROGRAMA                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE  ZEROS                 TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
