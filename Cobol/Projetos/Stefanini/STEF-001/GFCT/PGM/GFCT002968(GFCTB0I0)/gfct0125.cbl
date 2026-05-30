      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0125.
       AUTHOR.     ANDRE LUIZ LONGO.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0125                                    *
      *    PROGRAMADOR.:   ANDRE LUIZ LONGO        - CPM PATO BRANCO   *
      *    ANALISTA CPM:   KHARUZO I. LEITE        - CPM PATO BRANCO   *
      *    ANALISTA....:   MARCELO CREM            - PROCWORK /GP.50   *
      *    DATA........:   15/03/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CANCELAMENTO DE SOLICITACAO DE PERMISSAO PA-*
      *      RA ADESAO.                                                *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                        INCLUDE/BOOK     *
      *                   DB2PRD.PRMSS_ADSAO_PCOTE      GFCTB0C6       *
      *                   DB2PRD.PARM_SERVC_TARIF       GFCTB0A2       *
      *                   DB2PRD.PARM_DATA_PROCM        GFCTB0A1       *
      *                   DB2PRD.PRMSS_GRP_DEPDC        GFCTB0C7       *
      *                   DB2PRD.PRMSS_GRP_EMPR         GFCTB0C8       *
      *                   DB2PRD.PRMSS_GRP_MUN          GFCTB0C9       *
      *                   DB2PRD.PRMSS_GRP_PAB          GFCTB0D0       *
      *                   DB2PRD.PRMSS_GRP_PSTAL        GFCTB0D1       *
      *                   DB2PRD.PRMSS_GRP_SGMTO        GFCTB0D2       *
      *                   DB2PRD.PRMSS_GRP_UF           GFCTB0D3       *
      *                   DB2PRD.TPRMSS_GRP_CLI         GFCTB0I0       *
      *                   DB2PRD.V01UF                  DCITV002       *
      *                   DB2PRD.ADSAO_COMP_PCOTE       GFCTB001       *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTFZ - ENTRADA - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG1 - SAIDA   - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG2 - ENTRADA - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTG3 - SAIDA   - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTK9 - ENTRADA                                          *
      *    I#GFCTKA - SAIDA                                            *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5522 - VERIFICAR DISPONIBILIDADE DO ON-LINE.            *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    POOL1470 - CONSISTENCIA DE DATAS                           *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                   *
      *================================================================*
      *                    A L T E R A C A O                           *
      *----------------------------------------------------------------*
      *                                                                *
      *    ANALISTA     : MARCUS VINICIUS          - PROCWORK          *
      *    DATA         : 20/10/2007                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      TRATAR O CAMPO DE ORIGEM DA SOLICITACAO PARA PERMISSAO    *
      *      INDIVIDUAL, PODENDO SER:                                  *
      *      LEGADO, TARIFAS OU AMBOS.                                 *
      *                                                                *
BI0810*================================================================*
BI0810*                    A L T E R A C A O                           *
BI0810*----------------------------------------------------------------*
BI0810*                                                                *
BI0810*    ANALISTA     : UBIRAJARA MIRANDA(BIRA)  - PROCWORK          *
BI0810*    DATA         : AGO/2010                                     *
BI0810*                                                                *
BI0810*    OBJETIVO     :                                              *
BI0810*    - AJUSTA A PESQUISA DA TABELA GFCTB001 PARA POSSIVEL        *
BI0810*      CHAVE DUPLICADA (SQLCODE -811).                           *
BI0810*                                                                *
BI0412*----------------------------------------------------------------*
BI0412*                 U L T I M A   A L T E R A C A O                *
BI0412*----------------------------------------------------------------*
BI0412*        SONDA PROCWORK - CONSULTORIA - ALTERACAO - BI0412       *
BI0412*----------------------------------------------------------------*
BI0412*                                                                *
BI0412*    ANALISTA....:  UBIRAJARA(BIRA) - SONDA - IT                 *
BI0412*    DATA........:  04 / 2012                                    *
BI0412*    OBJETIVO....:  SETAR FLAG PARA PERMITIR VIZUALIZACAO        *
BI0412*                   DE ADESAO COMPULSORIA, A ADESAO COMPULSORIA  *
BI0412*                   ASSINALADA NO ONLINE SERA INSERIDA NA        *
BI0412*                   BASE GFCTB009 DE ADESAO INDIVIDUAL.          *
BI0412*                                                                *
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
           '* INICIO DA AREA DE WORKING *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA TO INDEXADORES *'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(003) COMP-3  VALUE ZEROS.
       77  IND-2                       PIC  9(003) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA TO VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-NOM-TAB                 PIC  X(011)         VALUE SPACES.

       01  WRK-DATA-AUX.
           05  WRK-DIA-AUX             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  WRK-MES-AUX             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  WRK-ANO-AUX             PIC  9(004)         VALUE ZEROS.

       01  WRK-DATA-INV.
           05  WRK-ANO-AUX             PIC  9(004)         VALUE ZEROS.
           05  WRK-MES-AUX             PIC  9(002)         VALUE ZEROS.
           05  WRK-DIA-AUX             PIC  9(002)         VALUE ZEROS.

       01  WRK-DATA-AUX2.
           05  WRK-DIA-AUX2            PIC  9(002)         VALUE ZEROS.
           05  WRK-MES-AUX2            PIC  9(002)         VALUE ZEROS.
           05  WRK-ANO-AUX2            PIC  9(004)         VALUE ZEROS.
       01  WRK-DATA-R                  REDEFINES WRK-DATA-AUX2
                                       PIC  9(008).

       01  WRK-DATA-FIM.
           05  WRK-ANO-FIM             PIC  9(004)         VALUE ZEROS.
           05  WRK-MES-FIM             PIC  9(002)         VALUE ZEROS.
           05  WRK-DIA-FIM             PIC  9(002)         VALUE ZEROS.

       01  WRK-DATA-FIM-ANT.
           05  WRK-ANO-FIM-ANT         PIC  9(004)         VALUE ZEROS.
           05  WRK-MES-FIM-ANT         PIC  9(002)         VALUE ZEROS.
           05  WRK-DIA-FIM-ANT         PIC  9(002)         VALUE ZEROS.

       01  WRK-DATA-INICIO.
           05  WRK-ANO-INI             PIC  9(004)         VALUE ZEROS.
           05  WRK-MES-INI             PIC  9(002)         VALUE ZEROS.
           05  WRK-DIA-INI             PIC  9(002)         VALUE ZEROS.

       01  WRK-DATA-DPROCM.
           05  WRK-ANO-PROCM           PIC  9(004)         VALUE ZEROS.
           05  WRK-MES-PROCM           PIC  9(002)         VALUE ZEROS.
           05  WRK-DIA-PROCM           PIC  9(002)         VALUE ZEROS.

       01  WRK-MSG001.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA AO MODULO '.
           05 WRK-MODULO-MSG           PIC  X(008)         VALUE SPACES.

BI0810 01  WRK-CFUNC-BDSCO.
BI0810     05  FILLER                  PIC  9(002)         VALUE ZEROS.
BI0810     05  WRK-FUNC-BDSCO          PIC  X(007)         VALUE SPACES.
BI0810 01  WRK-CFUNC-BDSCO-R           REDEFINES WRK-CFUNC-BDSCO
BI0810                                 PIC  9(009).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREAS DA POOL1470 *'.
      *----------------------------------------------------------------*

       01  WRK-DATA                    PIC  9(008)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
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
           '* AREA TO TABELA DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB001
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C6
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C7
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C8
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C9
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D0
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D2
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D3
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
             INCLUDE DCITV002
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A2
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0I0
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA AREA DE WORKING *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTK9'.

       COPY 'I#GFCTKA'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTK9-ENTRADA
                                                      GFCTKA-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *INICIA O PROGRAMA                                               *
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
      *MONTA AREA DE SAIDA, VERIFICA DISPONIBILIDADE DO SISTEMA E CON- *
      *SISTE DADOS.                                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          GFCTKA-SAIDA.
           MOVE ZEROS                  TO IND-1
                                          IND-2.

           INITIALIZE GFCTKA-SAIDA
                      GFCT0M-AREA-ERROS.

           PERFORM 1100-VERIFICAR-DISP-SISTEMA.

           PERFORM 1200-MONTAR-AREA-SAIDA.

           PERFORM 1300-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICA DISPONIBILIDADE DO SISTEMA                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-VERIFICAR-DISP-SISTEMA     SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG1-SAIDA.

           INITIALIZE GFCTG1-SAIDA.

           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE GFCTK9-TRANSACAO       TO GFCTFZ-TRANSACAO.
           MOVE GFCTK9-FUNCAO          TO GFCTFZ-FUNCAO.
           MOVE GFCTK9-FUNC-BDSCO      TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.

           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTKA-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0125'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO                 EQUAL 9
               MOVE 9                      TO GFCTKA-ERRO
               MOVE 'GFCT0125'             TO GFCT0M-TRANSACAO
               MOVE GFCTG1-COD-SQL-ERRO    TO GFCT0M-SQLCODE
                                              GFCTKA-COD-SQL-ERRO
               MOVE GFCTG1-ERRO            TO GFCTKA-ERRO
               MOVE GFCTG1-COD-MSG-ERRO    TO GFCTKA-COD-MSG-ERRO
               MOVE GFCTG1-DESC-MSG-ERRO   TO GFCT0M-TEXTO
                                              GFCTKA-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE          EQUAL 1
               MOVE 9                      TO GFCTKA-ERRO
               MOVE 'SISTEMA INDISPONIVEL' TO GFCT0M-TEXTO
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0125'             TO GFCT0M-TRANSACAO
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *MONTA AREA DE SAIDA                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-MONTAR-AREA-SAIDA          SECTION.
      *----------------------------------------------------------------*

SMS        MOVE 295                    TO GFCTKA-LL.
           MOVE ZEROS                  TO GFCTKA-ZZ.

           MOVE GFCTK9-TRANSACAO       TO GFCTKA-TRANSACAO.
           MOVE GFCTK9-FUNCAO          TO GFCTKA-FUNCAO.

           IF  GFCTK9-QTDE-OCOR        NOT NUMERIC
               MOVE ZEROS              TO GFCTKA-QTDE-OCOR
           ELSE
               MOVE GFCTK9-QTDE-OCOR       TO GFCTKA-QTDE-OCOR
           END-IF.

           MOVE ZEROS                  TO GFCTKA-QTDE-TOT-REG.

           MOVE GFCTK9-FUNC-BDSCO      TO GFCTKA-FUNC-BDSCO.
           MOVE GFCTK9-PACOTE          TO GFCTKA-PACOTE.
           MOVE GFCTK9-AGPTO           TO GFCTKA-AGPTO.

           MOVE 'S'                        TO GFCTKA-FIM.

           MOVE ZEROS                      TO GFCTKA-ERRO
                                              GFCTKA-COD-SQL-ERRO
                                              GFCTKA-COD-MSG-ERRO
                                              GFCTG2-COD-MSG.

           PERFORM 1310-OBTER-DESC-MSG.

           MOVE GFCTG3-DESC-MSG-ERRO       TO GFCTKA-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTE DADOS                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (GFCTK9-TRANSACAO        EQUAL SPACES OR LOW-VALUES) OR
              (GFCTK9-FUNCAO           EQUAL SPACES OR LOW-VALUES) OR
              (GFCTK9-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES) OR
              (GFCTK9-QTDE-OCOR        EQUAL ZEROS) OR
              (GFCTK9-QTDE-OCOR        NOT NUMERIC) OR
              (GFCTK9-QTDE-TOT-REG     NOT NUMERIC) OR
              (GFCTK9-PACOTE           EQUAL ZEROS) OR
              (GFCTK9-PACOTE           NOT NUMERIC) OR
              (GFCTK9-AGPTO            NOT EQUAL  4 AND  9 AND
                                                 14 AND 15 AND
                                                 16 AND 17 AND
                                                 18 AND 19 AND
                                                 20)
               MOVE  1                 TO GFCTKA-ERRO
               MOVE  ZEROS             TO GFCTKA-COD-SQL-ERRO
               MOVE  0001              TO GFCTKA-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTKA-DESC-MSG-ERRO

               PERFORM 1320-MONTAR-DADOS-OCORRENCIA

               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 1330-SELECIONAR-GFCTB0A1.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTEM DESCRICAO DA MENSAGEM                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1310-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE GFCTK9-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTK9-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTK9-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTKA-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0125'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 9                  TO GFCTKA-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0125'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO                 EQUAL 1
               MOVE 1                      TO GFCTKA-ERRO
               MOVE GFCTG3-COD-SQL-ERRO    TO GFCTKA-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO    TO GFCTKA-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO   TO GFCTKA-DESC-MSG-ERRO

               PERFORM 1320-MONTAR-DADOS-OCORRENCIA

               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *MONTAR DADOS DA OCORRENCIA DA AREA DE SAIDA                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1320-MONTAR-DADOS-OCORRENCIA    SECTION.
      *----------------------------------------------------------------*

           PERFORM
           VARYING IND-1               FROM 1 BY 1
             UNTIL IND-1               GREATER GFCTK9-QTDE-OCOR
                   PERFORM 2210-MOVIMENTA-DADOS
           END-PERFORM.

      *----------------------------------------------------------------*
       1320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *SELECIONAR DATA ATUAL DA TABELA GFCTB0A1                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1330-SELECIONAR-GFCTB0A1        SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

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
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0A1'      TO WRK-NOM-TAB
               MOVE 'GFCT0125'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTKA-ERRO
               MOVE  SQLCODE           TO GFCTKA-COD-SQL-ERRO
               MOVE SPACES             TO GFCTKA-DESC-MSG-ERRO
               MOVE  0012              TO GFCTKA-COD-MSG-ERRO
               PERFORM 1331-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO WRK-DATA-AUX.
           MOVE CORR WRK-DATA-AUX      TO WRK-DATA-INV.
           MOVE WRK-DATA-INV           TO WRK-DATA-DPROCM.

      *----------------------------------------------------------------*
       1330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FORMATA ERROS DB2                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       1331-FORMATAR-ERRO-DB2          SECTION.
      *----------------------------------------------------------------*

           STRING GFCTG3-DESC-MSG      WRK-NOM-TAB
           DELIMITED BY '  '           INTO GFCTKA-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1331-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAMENTO PRINCIPAL                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING IND-1       FROM 1 BY 1
               UNTIL IND-1             GREATER GFCTK9-QTDE-OCOR
                   PERFORM 2100-MONTA-DATA-INV
                   PERFORM 2200-AVALIA-OCORRENCIAS
           END-PERFORM.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *MONTAR DATAS INVERTIDAS                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-MONTA-DATA-INV             SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTK9-DATA-INICIO(IND-1)  TO WRK-DATA-AUX.
           MOVE CORR WRK-DATA-AUX          TO WRK-DATA-INV.
           MOVE WRK-DATA-INV               TO WRK-DATA-INICIO.
           MOVE GFCTK9-DATA-FIM(IND-1)     TO WRK-DATA-AUX.
           MOVE CORR WRK-DATA-AUX          TO WRK-DATA-INV.
           MOVE WRK-DATA-INV               TO WRK-DATA-FIM.
           MOVE GFCTK9-DATA-FIM-ANT(IND-1) TO WRK-DATA-AUX.
           MOVE CORR WRK-DATA-AUX          TO WRK-DATA-INV.
           MOVE WRK-DATA-INV               TO WRK-DATA-FIM-ANT.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *AVALIA OCORRENCIAS                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-AVALIA-OCORRENCIAS         SECTION.
      *----------------------------------------------------------------*

           IF  GFCTK9-DATA-FIM(IND-1)  EQUAL SPACES
               MOVE 0680               TO GFCTG2-COD-MSG
               PERFORM 2210-MOVIMENTA-DADOS
               GO TO 2200-99-FIM
           END-IF.

           MOVE GFCTK9-DATA-FIM(IND-1) TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            OF WRK-DATA-AUX
                                       TO WRK-DIA-AUX2.
           MOVE WRK-MES-AUX            OF WRK-DATA-AUX
                                       TO WRK-MES-AUX2.
           MOVE WRK-ANO-AUX            OF WRK-DATA-AUX
                                       TO WRK-ANO-AUX2.
           MOVE WRK-DATA-R             TO WRK-DATA.

           CALL 'POOL1470'             USING WRK-DATA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 0681               TO GFCTG2-COD-MSG
               PERFORM 2210-MOVIMENTA-DADOS
               GO TO 2200-99-FIM
           END-IF.

           IF  WRK-DATA-FIM            NOT LESS  WRK-DATA-FIM-ANT  AND
               WRK-DATA-FIM            NOT EQUAL  WRK-DATA-INICIO
               MOVE 0682               TO GFCTG2-COD-MSG
               PERFORM 2210-MOVIMENTA-DADOS
               GO TO 2200-99-FIM
           END-IF.

           IF  WRK-DATA-FIM            LESS WRK-DATA-INICIO
               MOVE 0683               TO GFCTG2-COD-MSG
               PERFORM 2210-MOVIMENTA-DADOS
               GO TO 2200-99-FIM
           END-IF.

           IF  WRK-DATA-FIM            LESS WRK-DATA-DPROCM
               MOVE 0684               TO GFCTG2-COD-MSG
               PERFORM 2210-MOVIMENTA-DADOS
               GO TO 2200-99-FIM
           END-IF.

           INSPECT GFCTK9-DATA-INICIO(IND-1) REPLACING ALL '/'  BY '.'.
           INSPECT GFCTK9-DATA-FIM(IND-1)    REPLACING ALL '/'  BY '.'.

BI0412     IF  GFCTK9-TP-ADESAO(IND-1) EQUAL 'C' OR 'A'
               PERFORM 2220-SELECIONAR-GFCTB001
BI0810         IF  (SQLCODE             EQUAL ZEROS  OR  -811)
                   MOVE 0739           TO GFCTG2-COD-MSG
                   PERFORM 2210-MOVIMENTA-DADOS
                   GO TO 2200-99-FIM
               END-IF
           END-IF.

           PERFORM 2230-CANCELAR-OCORRENCIA.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *MOVIMENTA DADOS                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-MOVIMENTA-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF  GFCTKA-ERRO                EQUAL  ZEROS
               MOVE 1                     TO GFCTKA-ERRO

               PERFORM 1310-OBTER-DESC-MSG

               MOVE GFCTG2-COD-MSG        TO GFCTKA-COD-MSG-ERRO
               MOVE ZEROS                 TO GFCTKA-COD-SQL-ERRO
               MOVE GFCTG3-DESC-MSG       TO GFCTKA-DESC-MSG-ERRO
           END-IF.

           ADD 1                          TO IND-2.
SMS        ADD 143                        TO GFCTKA-LL.

           ADD 1                          TO GFCTKA-QTDE-TOT-REG.

           MOVE GFCTK9-SEQUENCIA(IND-1)   TO GFCTKA-SEQUENCIA(IND-2).
           MOVE GFCTK9-CPF-CNPJ(IND-1)    TO GFCTKA-CPF-CNPJ(IND-2).
           MOVE GFCTK9-FILIAL(IND-1)      TO GFCTKA-FILIAL(IND-2).
           MOVE GFCTK9-CONTROLE(IND-1)    TO GFCTKA-CONTROLE(IND-2).
           MOVE GFCTK9-AGENCIA(IND-1)     TO GFCTKA-AGENCIA(IND-2).
           MOVE GFCTK9-CONTA(IND-1)       TO GFCTKA-CONTA(IND-2).
           MOVE GFCTK9-POSTO(IND-1)       TO GFCTKA-POSTO(IND-2).
           MOVE GFCTK9-SEGMENTO(IND-1)    TO GFCTKA-SEGMENTO(IND-2).
           MOVE GFCTK9-MUNICIPIO(IND-1)   TO GFCTKA-MUNICIPIO(IND-2).
           MOVE GFCTK9-UF(IND-1)          TO GFCTKA-UF(IND-2).
           MOVE GFCTK9-DESC-NOME(IND-1)   TO GFCTKA-DESC-NOME(IND-2).
           MOVE GFCTK9-TP-ADESAO(IND-1)   TO GFCTKA-TP-ADESAO(IND-2).
SMS        MOVE GFCTK9-SOLICITA-EXCECAO(IND-1)
SMS                                TO GFCTKA-SOLICITA-EXCECAO(IND-2).
           MOVE GFCTK9-DATA-INICIO-ANT(IND-1)
                                       TO GFCTKA-DATA-INICIO-ANT(IND-2).
           MOVE GFCTK9-DATA-FIM-ANT(IND-1)
                                          TO GFCTKA-DATA-FIM-ANT(IND-2).
           MOVE GFCTK9-DATA-INICIO(IND-1) TO GFCTKA-DATA-INICIO(IND-2).
           MOVE GFCTK9-DATA-FIM(IND-1)    TO GFCTKA-DATA-FIM(IND-2).

           IF  GFCTG2-COD-MSG             NOT EQUAL 0001
               MOVE 1                     TO GFCTKA-ERRO-OCOR(IND-2)
           END-IF.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FAZ SELECT NA TABELA GFCTB001                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2220-SELECIONAR-GFCTB001        SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTK9-PACOTE           TO CSERVC-TARIF     OF GFCTB001.
           MOVE GFCTK9-AGPTO            TO CAGPTO-CTA       OF GFCTB001.
           MOVE GFCTK9-DATA-INICIO(IND-1) TO DINIC-ADSAO-COMP
                                                            OF GFCTB001.
           MOVE GFCTK9-DATA-FIM(IND-1)  TO DFIM-ADSAO-COMP  OF GFCTB001.

           EXEC SQL
             SELECT
                   CSERVC_TARIF
             INTO
                   :GFCTB001.CSERVC-TARIF
             FROM   DB2PRD.ADSAO_COMP_PCOTE
             WHERE
                   CSERVC_TARIF       = :GFCTB001.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB001.CAGPTO-CTA         AND
                   DINIC_ADSAO_COMP  >= :GFCTB001.DINIC-ADSAO-COMP   AND
                   DFIM_ADSAO_COMP   <= :GFCTB001.DFIM-ADSAO-COMP
           END-EXEC.

BI0810     IF (SQLCODE             NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0            EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB001'      TO WRK-NOM-TAB
               MOVE 'GFCT0125'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'ADSAO_COMP_PCOTE' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTKA-ERRO
               MOVE  SQLCODE           TO GFCTKA-COD-SQL-ERRO
               MOVE SPACES             TO GFCTKA-DESC-MSG-ERRO
               MOVE  0012              TO GFCTKA-COD-MSG-ERRO
               PERFORM 1331-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CANCELAR OCORRENCIA                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2230-CANCELAR-OCORRENCIA        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-DATA-FIM             EQUAL  WRK-DATA-INICIO
               IF  WRK-DATA-INICIO      GREATER WRK-DATA-DPROCM
                   PERFORM 2231-EXCLUIR-FISICAMENTE
               ELSE
                   PERFORM 2232-EXCLUIR-LOGICAMENTE
               END-IF
           ELSE
               PERFORM 2232-EXCLUIR-LOGICAMENTE
           END-IF.

      *----------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EXCLUIR FISICAMENTE                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2231-EXCLUIR-FISICAMENTE        SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTK9-AGPTO
               WHEN 15
                   PERFORM 22311-DELETA-PAB
               WHEN 5
                   PERFORM 22312-DELETA-AGENCIA
               WHEN 16
                   PERFORM 22313-DELETA-EMPRESA
               WHEN 17
                   PERFORM 22314-DEL-POSTAL-EXPRE
               WHEN 20
                   PERFORM 22314-DEL-POSTAL-EXPRE
      *        WHEN 4
      *            PERFORM 22315-DELETA-CLIENTE
               WHEN 9
                   PERFORM 22316-DELETA-SEGMENTO
               WHEN 18
                   PERFORM 22317-DELETA-MUNICIPIO
               WHEN 19
                   PERFORM 22318-DELETA-UF
           END-EVALUATE.

           PERFORM 22319-DELETA-PRINCIPAL.

      *----------------------------------------------------------------*
       2231-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *DELETAR-PAB                                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       22311-DELETA-PAB                SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTK9-PACOTE             TO CSERVC-TARIF   OF GFCTB0D0.
           MOVE GFCTK9-AGPTO              TO CAGPTO-CTA     OF GFCTB0D0.
           MOVE GFCTK9-DATA-INICIO(IND-1)
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0D0.
           MOVE GFCTK9-SEQUENCIA(IND-1) TO CSEQ-AGPTO-CTA   OF GFCTB0D0.

           EXEC SQL
             DELETE FROM    DB2PRD.PRMSS_GRP_PAB
             WHERE
                   CSERVC_TARIF       = :GFCTB0D0.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB0D0.CAGPTO-CTA         AND
                   DINIC_PRMSS_PCOTE  = :GFCTB0D0.DINIC-PRMSS-PCOTE  AND
                   CSEQ_AGPTO_CTA     = :GFCTB0D0.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0020'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D0'      TO WRK-NOM-TAB
               MOVE 'GFCT0125'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PRMSS_GRP_PAB'    TO GFCT0M-NOME-TAB
               MOVE 'DELETE'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTKA-ERRO
               MOVE  SQLCODE           TO GFCTKA-COD-SQL-ERRO
               MOVE SPACES             TO GFCTKA-DESC-MSG-ERRO
               MOVE  0020              TO GFCTKA-COD-MSG-ERRO
               PERFORM 1331-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       21311-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *DELETAR-AGENCIA                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       22312-DELETA-AGENCIA            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTK9-PACOTE           TO CSERVC-TARIF   OF GFCTB0C7.
           MOVE GFCTK9-AGPTO            TO CAGPTO-CTA     OF GFCTB0C7.
           MOVE GFCTK9-DATA-INICIO(IND-1)TO DINIC-PRMSS-PCOTE
                                                          OF GFCTB0C7.
           MOVE GFCTK9-SEQUENCIA(IND-1) TO CSEQ-AGPTO-CTA OF GFCTB0C7.

           EXEC SQL
             DELETE FROM    DB2PRD.PRMSS_GRP_DEPDC
             WHERE
                   CSERVC_TARIF       = :GFCTB0C7.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB0C7.CAGPTO-CTA         AND
                   DINIC_PRMSS_PCOTE  = :GFCTB0C7.DINIC-PRMSS-PCOTE  AND
                   CSEQ_AGPTO_CTA     = :GFCTB0C7.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0020'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C7'      TO WRK-NOM-TAB
               MOVE 'GFCT0125'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PRMSS_GRP_DEPDC'  TO GFCT0M-NOME-TAB
               MOVE 'DELETE'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTKA-ERRO
               MOVE  SQLCODE           TO GFCTKA-COD-SQL-ERRO
               MOVE SPACES             TO GFCTKA-DESC-MSG-ERRO
               MOVE  0020              TO GFCTKA-COD-MSG-ERRO
               PERFORM 1331-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       21312-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *DELETAR-EMPRESA                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       22313-DELETA-EMPRESA            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTK9-PACOTE           TO CSERVC-TARIF     OF GFCTB0C8.
           MOVE GFCTK9-AGPTO            TO CAGPTO-CTA       OF GFCTB0C8.
           MOVE GFCTK9-DATA-INICIO(IND-1) TO DINIC-PRMSS-PCOTE
                                                            OF GFCTB0C8.
           MOVE GFCTK9-SEQUENCIA(IND-1) TO CSEQ-AGPTO-CTA   OF GFCTB0C8.

           EXEC SQL
             DELETE FROM    DB2PRD.PRMSS_GRP_EMPR
             WHERE
                   CSERVC_TARIF       = :GFCTB0C8.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB0C8.CAGPTO-CTA         AND
                   DINIC_PRMSS_PCOTE  = :GFCTB0C8.DINIC-PRMSS-PCOTE  AND
                   CSEQ_AGPTO_CTA     = :GFCTB0C8.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0020'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C8'      TO WRK-NOM-TAB
               MOVE 'GFCT0125'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PRMSS_GRP_EMPR'   TO GFCT0M-NOME-TAB
               MOVE 'DELETE'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTKA-ERRO
               MOVE  SQLCODE           TO GFCTKA-COD-SQL-ERRO
               MOVE SPACES             TO GFCTKA-DESC-MSG-ERRO
               MOVE  0020              TO GFCTKA-COD-MSG-ERRO
               PERFORM 1331-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       21313-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *DELETAR POSTAL                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       22314-DEL-POSTAL-EXPRE          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTK9-PACOTE           TO CSERVC-TARIF     OF GFCTB0D1.
           MOVE GFCTK9-AGPTO            TO CAGPTO-CTA       OF GFCTB0D1.
           MOVE GFCTK9-DATA-INICIO(IND-1) TO DINIC-PRMSS-PCOTE
                                                            OF GFCTB0D1.
           MOVE GFCTK9-SEQUENCIA(IND-1) TO CSEQ-AGPTO-CTA   OF GFCTB0D1.

           EXEC SQL
             DELETE FROM  DB2PRD.PRMSS_GRP_PSTAL
             WHERE
                   CSERVC_TARIF       = :GFCTB0D1.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB0D1.CAGPTO-CTA         AND
                   DINIC_PRMSS_PCOTE  = :GFCTB0D1.DINIC-PRMSS-PCOTE  AND
                   CSEQ_AGPTO_CTA     = :GFCTB0D1.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0020'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D1'      TO WRK-NOM-TAB
               MOVE 'GFCT0125'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PRMSS_GRP_PSTAL'  TO GFCT0M-NOME-TAB
               MOVE 'DELETE'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTKA-ERRO
               MOVE  SQLCODE           TO GFCTKA-COD-SQL-ERRO
               MOVE SPACES             TO GFCTKA-DESC-MSG-ERRO
               MOVE  0020              TO GFCTKA-COD-MSG-ERRO
               PERFORM 1331-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       21314-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *DELETAR CLIENTE                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       22315-DELETA-CLIENTE            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTK9-PACOTE           TO CSERVC-TARIF     OF GFCTB0I0.
           MOVE GFCTK9-AGPTO            TO CAGPTO-CTA       OF GFCTB0I0.
           MOVE GFCTK9-DATA-INICIO(IND-1) TO DINIC-PRMSS-PCOTE
                                                            OF GFCTB0I0.
           MOVE GFCTK9-SEQUENCIA(IND-1) TO CSEQ-AGPTO-CTA   OF GFCTB0I0.

           EXEC SQL  DELETE FROM DB2PRD.TPRMSS_GRP_CLI
            WHERE CSERVC_TARIF      = :GFCTB0I0.CSERVC-TARIF
            AND   CAGPTO_CTA        = :GFCTB0I0.CAGPTO-CTA
            AND   DINIC_PRMSS_PCOTE = :GFCTB0I0.DINIC-PRMSS-PCOTE
            AND   CSEQ_AGPTO_CTA    = :GFCTB0I0.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 0020               TO GFCTKA-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               MOVE 'TPRMSS_GRP_CLI'   TO GFCT0M-NOME-TAB
               MOVE 'DELETE'           TO GFCT0M-COMANDO-SQL
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE 'GFCTB0I0'         TO WRK-NOM-TAB
               MOVE SPACES             TO GFCTKA-DADOS-RST
               PERFORM 1331-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       21315-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *DELETAR SEGMENTO                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       22316-DELETA-SEGMENTO           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTK9-PACOTE           TO CSERVC-TARIF     OF GFCTB0D2.
           MOVE GFCTK9-AGPTO            TO CAGPTO-CTA       OF GFCTB0D2.
           MOVE GFCTK9-DATA-INICIO(IND-1) TO DINIC-PRMSS-PCOTE
                                                            OF GFCTB0D2.
           MOVE GFCTK9-SEQUENCIA(IND-1) TO CSEQ-AGPTO-CTA   OF GFCTB0D2.

           EXEC SQL
             DELETE FROM    DB2PRD.PRMSS_GRP_SGMTO
             WHERE
                   CSERVC_TARIF       = :GFCTB0D2.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB0D2.CAGPTO-CTA         AND
                   DINIC_PRMSS_PCOTE  = :GFCTB0D2.DINIC-PRMSS-PCOTE  AND
                   CSEQ_AGPTO_CTA     = :GFCTB0D2.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0020'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D2'      TO WRK-NOM-TAB
               MOVE 'GFCT0125'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PRMSS_GRP_SGMTO'  TO GFCT0M-NOME-TAB
               MOVE 'DELETE'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTKA-ERRO
               MOVE  SQLCODE           TO GFCTKA-COD-SQL-ERRO
               MOVE SPACES             TO GFCTKA-DESC-MSG-ERRO
               MOVE  0020              TO GFCTKA-COD-MSG-ERRO
               PERFORM 1331-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       21316-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *DELETAR MUNICIPIO                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       22317-DELETA-MUNICIPIO          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTK9-PACOTE           TO CSERVC-TARIF    OF GFCTB0C9.
           MOVE GFCTK9-AGPTO            TO CAGPTO-CTA      OF GFCTB0C9.
           MOVE GFCTK9-DATA-INICIO(IND-1) TO DINIC-PRMSS-PCOTE
                                                           OF GFCTB0C9.
           MOVE GFCTK9-SEQUENCIA(IND-1) TO CSEQ-AGPTO-CTA  OF GFCTB0C9.

           EXEC SQL
             DELETE FROM DB2PRD.PRMSS_GRP_MUN
             WHERE
                   CSERVC_TARIF       = :GFCTB0C9.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB0C9.CAGPTO-CTA         AND
                   DINIC_PRMSS_PCOTE  = :GFCTB0C9.DINIC-PRMSS-PCOTE  AND
                   CSEQ_AGPTO_CTA     = :GFCTB0C9.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0020'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C9'      TO WRK-NOM-TAB
               MOVE 'GFCT0125'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PRMSS_GRP_MUN'    TO GFCT0M-NOME-TAB
               MOVE 'DELETE'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0090'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTKA-ERRO
               MOVE  SQLCODE           TO GFCTKA-COD-SQL-ERRO
               MOVE SPACES             TO GFCTKA-DESC-MSG-ERRO
               MOVE  0020              TO GFCTKA-COD-MSG-ERRO
               PERFORM 1331-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       21317-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *DELETAR UF                                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       22318-DELETA-UF                 SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTK9-PACOTE           TO CSERVC-TARIF     OF GFCTB0D3.
           MOVE GFCTK9-AGPTO            TO CAGPTO-CTA       OF GFCTB0D3.
           MOVE GFCTK9-DATA-INICIO(IND-1) TO DINIC-PRMSS-PCOTE
                                                            OF GFCTB0D3.
           MOVE GFCTK9-SEQUENCIA(IND-1) TO CSEQ-AGPTO-CTA   OF GFCTB0D3.

           EXEC SQL
             DELETE FROM    DB2PRD.PRMSS_GRP_UF
             WHERE
                   CSERVC_TARIF       = :GFCTB0D3.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB0D3.CAGPTO-CTA         AND
                   DINIC_PRMSS_PCOTE  = :GFCTB0D3.DINIC-PRMSS-PCOTE  AND
                   CSEQ_AGPTO_CTA     = :GFCTB0D3.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0020'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D3'      TO WRK-NOM-TAB
               MOVE 'GFCT0125'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PRMSS_GRP_UF'     TO GFCT0M-NOME-TAB
               MOVE 'DELETE'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0100'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTKA-ERRO
               MOVE  SQLCODE           TO GFCTKA-COD-SQL-ERRO
               MOVE SPACES             TO GFCTKA-DESC-MSG-ERRO
               MOVE  0020              TO GFCTKA-COD-MSG-ERRO
               PERFORM 1331-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       21318-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *DELETAR PRINCIPAL                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       22319-DELETA-PRINCIPAL          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTK9-PACOTE           TO CSERVC-TARIF    OF GFCTB0C6.
           MOVE GFCTK9-AGPTO            TO CAGPTO-CTA      OF GFCTB0C6.
           MOVE GFCTK9-DATA-INICIO(IND-1) TO DINIC-PRMSS-PCOTE
                                                           OF GFCTB0C6.
           MOVE GFCTK9-SEQUENCIA(IND-1) TO CSEQ-AGPTO-CTA  OF GFCTB0C6.

           EXEC SQL
             DELETE FROM    DB2PRD.PRMSS_ADSAO_PCOTE
             WHERE
                   CSERVC_TARIF       = :GFCTB0C6.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB0C6.CAGPTO-CTA         AND
                   DINIC_PRMSS_PCOTE  = :GFCTB0C6.DINIC-PRMSS-PCOTE  AND
                   CSEQ_AGPTO_CTA     = :GFCTB0C6.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0020'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C6'      TO WRK-NOM-TAB
               MOVE 'GFCT0125'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PRMSS_GRP_PCOTE'  TO GFCT0M-NOME-TAB
               MOVE 'DELETE'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0110'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTKA-ERRO
               MOVE  SQLCODE           TO GFCTKA-COD-SQL-ERRO
               MOVE SPACES             TO GFCTKA-DESC-MSG-ERRO
               MOVE  0020              TO GFCTKA-COD-MSG-ERRO
               PERFORM 1331-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       21319-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EXCLUIR LOGICAMENTE                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2232-EXCLUIR-LOGICAMENTE        SECTION.
      *----------------------------------------------------------------*

           INSPECT GFCTK9-DATA-FIM(IND-1) REPLACING ALL '/' BY '.'.
           MOVE   GFCTK9-DATA-FIM(IND-1)  TO DFIM-PRMSS-PCOTE
                                                            OF GFCTB0C6.
           MOVE GFCTK9-PACOTE             TO CSERVC-TARIF   OF GFCTB0C6.
           MOVE GFCTK9-AGPTO              TO CAGPTO-CTA     OF GFCTB0C6.
           MOVE GFCTK9-DATA-INICIO(IND-1) TO DINIC-PRMSS-PCOTE
                                                            OF GFCTB0C6.
           MOVE GFCTK9-SEQUENCIA(IND-1)   TO CSEQ-AGPTO-CTA OF GFCTB0C6.
BI0810     MOVE GFCTK9-FUNC-BDSCO         TO WRK-FUNC-BDSCO.
BI0810     MOVE WRK-CFUNC-BDSCO-R         TO CFUNC-MANUT    OF GFCTB0C6.

           EXEC SQL
             UPDATE   DB2PRD.PRMSS_ADSAO_PCOTE
               SET
                   DFIM_PRMSS_PCOTE   = :GFCTB0C6.DFIM-PRMSS-PCOTE,
BI0810             CFUNC_MANUT        = :GFCTB0C6.CFUNC-MANUT,
BI0810             HMANUT_REG         = CURRENT TIMESTAMP
             WHERE
                   CSERVC_TARIF       = :GFCTB0C6.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB0C6.CAGPTO-CTA         AND
                   DINIC_PRMSS_PCOTE  = :GFCTB0C6.DINIC-PRMSS-PCOTE  AND
                   CSEQ_AGPTO_CTA     = :GFCTB0C6.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0019'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C6'      TO WRK-NOM-TAB
               MOVE 'GFCT0125'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PRMSS_GRP_PCOTE'  TO GFCT0M-NOME-TAB
               MOVE 'UPDATE'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0120'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTKA-ERRO
               MOVE  SQLCODE           TO GFCTKA-COD-SQL-ERRO
               MOVE SPACES             TO GFCTKA-DESC-MSG-ERRO
               MOVE  0019              TO GFCTKA-COD-MSG-ERRO
               PERFORM 1331-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2132-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *RETORNA AO CHAMADOR                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
