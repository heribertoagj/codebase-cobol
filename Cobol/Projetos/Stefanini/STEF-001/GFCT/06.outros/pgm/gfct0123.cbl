      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT0123.
       AUTHOR.     ANDRE LUIZ LONGO.
      *================================================================*
      *                    C P M  -  S I S T E M A S.                  *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0123                                    *
      *    PROGRAMADOR.:   ANDRE LUIZ LONGO        - CPM PATO BRANCO   *
      *    ANALISTA CPM:   JOSSIANE CARLA GAZZONI  - CPM PATO BRANCO   *
      *    ANALISTA....:   MARCELO CREM            - PROCWORK /GP.50   *
      *    DATA........:   15/03/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   INCLUSAO DE SOLICITACAO DE PERMISSAO PARA   *
      *      ADESAO.                                                   *
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
      *                   DB2PRD.PRMSS_GRP_PST          GFCTB0D1       *
      *                   DB2PRD.PRMSS_GRP_SGMTO        GFCTB0D2       *
      *                   DB2PRD.SERVC_TARIF_PRINC      GFCTB0D8       *
      *                   DB2PRD.PRMSS_GRP_UF           GFCTB0D3       *
      *                   DB2PRD.TPRMSS_CLI             GFCTB0I0       *
      *                   DB2PRD.V01UF                  DCITV002       *
      *                   DB2PRD.TCAD_CORSP_BCRIO       YMKCB001       *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTFZ - ENTRADA - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG1 - SAIDA   - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG2 - ENTRADA - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTG3 - SAIDA   - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTK7 - ENTRADA                                          *
      *    I#GFCTK8 - SAIDA                                            *
      *    I#GFCTIU - ENTRADA - ENTRADA DO MODULO GFCT5110.            *
      *    I#GFCTIV - SAIDA   - SAIDA DO MODULO GFCT5110.              *
      *    I#GFCTBR - ENTRADA - ENTRADA DO MODULO GFCT5060.            *
      *    I#GFCTBS - SAIDA   - SAIDA DO MODULO GFCT5060.              *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *    I#MESUM6 - AREA PARA O MODULO MESU9015.                     *
      *    I#MESUM8 - AREA PARA O MODULO MESU9018.                     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5522 - VERIFICAR DISPONIBILIDADE DO ON-LINE.            *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    GFCT5508 - OBTER NOME CLIENTE                               *
      *    GFCT5110 - OBTER NOME CLIENTE POR CNPJ/CPF                  *
      *    MESU9015 - CONSISTIR AGENCIA                                *
      *    MESU9018 - CONSISTIR PAB                                    *
      *    GFCT5060 - CONSISTIR AGENCIA E CONTA                        *
      *    DCIT8000 - VERIFICA UF                                      *
      *    RURC9020 - VERIFICA MUNICIPIO                               *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                   *
      *    POOL0110 - CONSISTIR CLIENTE (CPF)                          *
      *================================================================*
      *                    A L T E R A C A O                           *
      *----------------------------------------------------------------*
      *                                                                *
      *    ANALISTA     : MARCUS VINICIUS          - PROCWORK          *
      *    DATA         : 20/10/2007                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      INCLUSAO DO CAMPO DE ORIGEM DA SOLICITACAO PARA PERMISSAO *
      *      INDIVIDUAL, PODENDO SER:                                  *
      *      LEGADO, TARIFAS OU AMBOS.                                 *
      *                                                                *
      *================================================================*
      *                    A L T E R A C A O                           *
      *----------------------------------------------------------------*
      *                                                                *
      *    ANALISTA     : MARCUS VINICIUS          - PROCWORK          *
      *    DATA         : 20/08/2008                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      INCLUSAO DO TRATAMENTO PARA BRADESCO EXPRESSO, ACESSANDO  *
      *      A BASE YMKCB001 DE CORRESPONDENTE BANCARIO, SEPARANDO-O   *
      *      DO TRATAMENTO DE BANCO POSTAL.                            *
      *                                                                *
      *================================================================*
MAR10 *                    A L T E R A C A O                           *
MAR10 *----------------------------------------------------------------*
MAR10 *                                                                *
MAR10 *    ANALISTA     : MARCUS VINICIUS          - PROCWORK          *
MAR10 *    DATA         : MAR/2010                                     *
MAR10 *                                                                *
MAR10 *    OBJETIVO     :                                              *
MAR10 *      INCLUSAO DO TRATAMENTO DO CAMPO GERA CREDITO NO CELULAR.  *
MAR10 *                                                                *
MAR10 *================================================================*
MAI10 *                    A L T E R A C A O                           *
MAI10 *----------------------------------------------------------------*
MAI10 *                                                                *
MAI10 *    ANALISTA     : MARCUS VINICIUS          - PROCWORK          *
MAI10 *    DATA         : MAI/2010                                     *
MAI10 *                                                                *
MAI10 *    OBJETIVO     :                                              *
MAI10 *      AJUSTE NA VALIDACAO DA DUPLICIDADE E FUTURO PARA OS AGRU- *
MAI10 *      PAMENTOS QUE UTILIZAM TOTAL SIM.                          *
MAI10 *                                                                *
BI0810*================================================================*
BI0810*                    A L T E R A C A O                           *
BI0810*----------------------------------------------------------------*
BI0810*                                                                *
BI0810*    ANALISTA     : UBIRAJARA MIRANDA(BIRA)  - PROCWORK          *
BI0810*    DATA         : AGO/2010                                     *
BI0810*                                                                *
BI0810*    OBJETIVO     :                                              *
BI0810*    - INCLUSAO DO CAMPO (TIPO DE PESSOA) NA TABELA GFCTB0C6,    *
BI0810*      CONSISTE PERMISSAO PARA FISICA, JURIDICA OU AMBAS.        *
BI0810*                                                                *
BI0810*================================================================*
BI0511*----------------------------------------------------------------*
BI0511*                 U L T I M A   A L T E R A C A O                *
BI0511*----------------------------------------------------------------*
BI0511*        SONDA PROCWORK - CONSULTORIA - ALTERACAO - BI0511       *
BI0511*----------------------------------------------------------------*
BI0511*                                                                *
BI0511*    ANALISTA....:  UBIRAJARA(BIRA) - SONDA/PROCWORK             *
BI0511*    DATA........:  05 / 2011                                    *
BI0511*    OBJETIVO....:  PROJETO CARTAO BONUS CELULAR - FASE II       *
BI0511*                                                                *
BI0412*================================================================*
BI0412*----------------------------------------------------------------*
BI0412*                 U L T I M A   A L T E R A C A O                *
BI0412*----------------------------------------------------------------*
BI0412*        SONDA PROCWORK - CONSULTORIA - ALTERACAO - BI0412       *
BI0412*----------------------------------------------------------------*
BI0412*                                                                *
BI0412*    ANALISTA....:  UBIRAJARA(BIRA) - SONDA/PROCWORK             *
BI0412*    DATA........:  04 / 2012                                    *
BI0412*    OBJETIVO....:  SETAR FLAG PARA PERMITIR VIZUALIZACAO        *
BI0412*                   DE ADESAO COMPULSORIA, A ADESAO COMPULSORIA  *
BI0412*                   ASSINALADA NO ONLINE SERA INSERIDA NA        *
BI0412*                   BASE GFCTB009 DE ADESAO INDIVIDUAL NO        *
BI0412*                   PROCESSAMENTO BATCH MENSAL.                  *
BI0412*                                                                *
      *================================================================*
      *                                                                *
      *    ANALISTA....:  CIBELE          - SONDA IT                   *
      *    DATA........:  05 / 2014                                    *
      *    OBJETIVO....:  SUBSTITUIR O ACESSO A BASE YMKCB001          *
      *                   PELO MODULO KCIM0184.                        *
      *                                                                *
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
           '* AREA TO VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-NOM-TAB                 PIC  X(011)         VALUE SPACES.
       77  WRK-FIM                     PIC  X(001)         VALUE SPACES.
       77  WRK-ACHOU-DUPLICIDADE       PIC  X(001)         VALUE SPACES.
       77  WRK-ACHOU-FUTURO            PIC  X(001)         VALUE SPACES.
       77  WRK-SEQUENCIA               PIC  9(009)         VALUE ZEROS.

       01  WRK-FUNC-BDSCO-X.
           05  WRK-FUNC-BDSCO          PIC  X(007)         VALUE SPACES.
       01  WRK-FUNC-BDSCO-R            REDEFINES WRK-FUNC-BDSCO-X
                                       PIC  9(007).

       01  WRK-CONTROLE-X.
           05  WRK-CONTROLE            PIC  X(002)         VALUE SPACES.
       01  WRK-CONTROLE-R              REDEFINES WRK-CONTROLE-X
                                       PIC  9(002).

       01  WRK-DATA-AUX.
           05  WRK-DIA-AUX             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  WRK-MES-AUX             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  WRK-ANO-AUX             PIC  9(004)         VALUE ZEROS.

       01  WRK-DATA-FIM.
           05  WRK-ANO-FIM             PIC  9(004)         VALUE ZEROS.
           05  WRK-MES-FIM             PIC  9(002)         VALUE ZEROS.
           05  WRK-DIA-FIM             PIC  9(002)         VALUE ZEROS.

       01  WRK-DATA-INICIO.
           05  WRK-ANO-INI             PIC  9(004)         VALUE ZEROS.
           05  WRK-MES-INI             PIC  9(002)         VALUE ZEROS.
           05  WRK-DIA-INI             PIC  9(002)         VALUE ZEROS.

       01  WRK-DATA-PROCM.
           05  WRK-ANO-PROCM           PIC  9(004)         VALUE ZEROS.
           05  WRK-MES-PROCM           PIC  9(002)         VALUE ZEROS.
           05  WRK-DIA-PROCM           PIC  9(002)         VALUE ZEROS.

       01  WRK-CAD-DATA.
           05  WRK-CAD-DIA             PIC  9(002)         VALUE ZEROS.
           05  WRK-CAD-MES             PIC  9(002)         VALUE ZEROS.
           05  WRK-CAD-ANO             PIC  9(004)         VALUE ZEROS.

       01  WRK-MSG001.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA AO MODULO '.
           05 WRK-MODULO-MSG           PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA DE COMUNICACAO COM KCIM0184           *'.
      *----------------------------------------------------------------*

       COPY 'KCIMW184'.

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
           '* AREA DE COMUNICACAO COM GFCT     - ENTRADA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTIU'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT     - SAIDA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTIV'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT     - ENTRADA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTBR'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT     - SAIDA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTBS'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM MESU9018 *'.
      *----------------------------------------------------------------*

       COPY 'I#MESUM8'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM MESU9015 *'.
      *----------------------------------------------------------------*

       COPY 'I#MESUM6'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM RURC9020 *'.
      *----------------------------------------------------------------*

       COPY 'I#RURC88'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM DCIT8000  *'.
      *----------------------------------------------------------------*

       01  WRK-DCIT-UNIDADE-FEDERATIVA.
           05  WRK-AREA-DCLGEN-DCITV002    PIC  X(070)     VALUE SPACES.
           05  WRK-DCIT-CODIGO-RETORNO     PIC  9(002)     VALUE ZEROS.

       01  WRK-ERRO-AREA                   PIC  X(107)     VALUE SPACES.
       01  WRK-SQLCA-DCIT                  PIC  X(136)     VALUE SPACES.

ST2506*----------------------------------------------------------------*
ST2506*01  FILLER                      PIC  X(050)         VALUE
ST2506*    '*** AREAS DA POOL0110 ***'.
ST2506*----------------------------------------------------------------*
ST2506*
ST2506*01  WRK-CGC-CPF-0110.
ST2506*    03  WRK-NUMERO-0110         PIC  9(009)         VALUE ZEROS.
ST2506*    03  WRK-FILIAL-0110         PIC  9(005)         VALUE ZEROS.
ST2506*01  WRK-CONTROLE-0110       PIC  X(002)             VALUE SPACES.

ST2506*----------------------------------------------------------------*
ST2506 01  FILLER                      PIC  X(050)         VALUE
ST2506    '*** AREAS DA BRAD2000 ***'.
ST2506*----------------------------------------------------------------*
ST2506
ST2506 01  WRK-BRAD2000                 PIC  X(08)  VALUE
ST2506     'BRAD2000'.
ST2506 
ST2506 01 WRK-AREA-BRAD2000.
ST2506    05 WRK-CAD-CPFCGC.
ST2506       10 WRK-CAD-NUMERO         PIC X(09)           VALUE SPACES.
ST2506       10 WRK-CAD-FILIAL         PIC X(04)           VALUE SPACES.
ST2506 77  WRK-CAD-CONTROLE            PIC 9(02)           VALUE ZEROS.
ST2506 01  WRK-9-X-2.
ST2506     05 WRK-9-02                 PIC 9(02)           VALUE ZEROS.

       01  WRK-CPAB-9-05               PIC  9(005)         VALUE ZEROS.
       01  WRK-FILLER                  REDEFINES WRK-CPAB-9-05.
           05 FILLER                   PIC  9(02).
           05 WRK-CPAB-9-03            PIC  9(03).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA TO TABELA DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C6
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D8
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
           '* AREA TO CURSORES *'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE   CSR01-GFCTB0C6   CURSOR   FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CSEQ_AGPTO_CTA,
BI0810             CINDCD_ADSAO_INDVD,
BI0412             CINDCD_VSLAO_COMP,
BI0810             CPSSOA_SERVC_TARIF
             FROM  DB2PRD.PRMSS_ADSAO_PCOTE
             WHERE
                   CAGPTO_CTA         = :GFCTB0C6.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0C6.CSERVC-TARIF       AND
                   DFIM_PRMSS_PCOTE   > :GFCTB0C6.DFIM-PRMSS-PCOTE
           END-EXEC.

           EXEC SQL
             DECLARE   CSR02-GFCTB0C6   CURSOR   FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CSEQ_AGPTO_CTA,
BI0810             CINDCD_ADSAO_INDVD,
BI0412             CINDCD_VSLAO_COMP,
BI0810             CPSSOA_SERVC_TARIF
             FROM  DB2PRD.PRMSS_ADSAO_PCOTE
             WHERE
                   CAGPTO_CTA         = :GFCTB0C6.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0C6.CSERVC-TARIF       AND
                   DINIC_PRMSS_PCOTE  > :GFCTB0C6.DINIC-PRMSS-PCOTE
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA AREA DE WORKING *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTK7'.

       COPY 'I#GFCTK8'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTK7-ENTRADA
                                                      GFCTK8-SAIDA
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
                                          GFCTK8-SAIDA.

           INITIALIZE GFCTK8-SAIDA
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
           MOVE GFCTK7-TRANSACAO       TO GFCTFZ-TRANSACAO.
           MOVE GFCTK7-FUNCAO          TO GFCTFZ-FUNCAO.
           MOVE GFCTK7-FUNC-BDSCO      TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.

           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTK8-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0123'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO                 EQUAL 9
               MOVE 9                      TO GFCTK8-ERRO
               MOVE 'GFCT0123'             TO GFCT0M-TRANSACAO
               MOVE GFCTG1-COD-SQL-ERRO    TO GFCT0M-SQLCODE
                                              GFCTK8-COD-SQL-ERRO
               MOVE GFCTG1-ERRO            TO GFCTK8-ERRO
               MOVE GFCTG1-COD-MSG-ERRO    TO GFCTK8-COD-MSG-ERRO
               MOVE GFCTG1-DESC-MSG-ERRO   TO GFCT0M-TEXTO
                                              GFCTK8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE          EQUAL 1
               MOVE 9                      TO GFCTK8-ERRO
               MOVE 'SISTEMA INDISPONIVEL' TO GFCT0M-TEXTO
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0123'             TO GFCT0M-TRANSACAO
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

           MOVE 1010                       TO GFCTK8-LL.
           MOVE GFCTK7-ZZ                  TO GFCTK8-ZZ.

           MOVE GFCTK7-TRANSACAO           TO GFCTK8-TRANSACAO.
           MOVE GFCTK7-FUNCAO              TO GFCTK8-FUNCAO.
           MOVE GFCTK7-TERMINAL            TO GFCTK8-TERMINAL.
           MOVE GFCTK7-FUNC-BDSCO          TO GFCTK8-FUNC-BDSCO.
           MOVE GFCTK7-FIXO                TO GFCTK8-FIXO.

           MOVE 'S'                        TO GFCTK8-FIM.

           MOVE ZEROS                      TO GFCTK8-ERRO
                                              GFCTK8-COD-SQL-ERRO
                                              GFCTK8-COD-MSG-ERRO
                                              GFCTG2-COD-MSG.

           PERFORM 1310-OBTER-DESC-MSG.

           MOVE GFCTG3-DESC-MSG            TO GFCTK8-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTE DADOS                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (GFCTK7-TRANSACAO        EQUAL SPACES OR LOW-VALUES) OR
              (GFCTK7-FUNCAO           EQUAL SPACES OR LOW-VALUES) OR
              (GFCTK7-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES) OR
              (GFCTK7-DATA-INICIO      EQUAL SPACES OR LOW-VALUES) OR
              (GFCTK7-DATA-FIM         EQUAL SPACES OR LOW-VALUES) OR
BI0412        (GFCTK7-TIPO-ADESAO  NOT EQUAL 'I' AND 'C' AND 'A') OR
BI0412        (GFCTK7-SOLICITA-EXCECAO NOT EQUAL 0 AND 1 ) OR
BI0412        (GFCTK7-PRMSS-INDVD      NOT EQUAL 0 AND 1 AND 2 AND 3) OR
              (GFCTK7-CPACOTE          EQUAL ZEROS) OR
              (GFCTK7-CAGPTO           NOT EQUAL 4 AND 9 AND 14 AND 15
PW1334                            AND 16 AND 17 AND 18 AND 19 AND 20)
               MOVE  1                 TO GFCTK8-ERRO
               MOVE  ZEROS             TO GFCTK8-COD-SQL-ERRO
               MOVE  0001              TO GFCTK8-COD-MSG-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTK8-FIM
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTK8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

BI0412     IF GFCTK7-TIPO-ADESAO    EQUAL 'C'  OR  'A'
BI0810        IF (GFCTK7-CPSSOA-TARIFA NOT EQUAL '1' AND '2' AND '3')
BI0810           MOVE  1                 TO GFCTK8-ERRO
BI0810           MOVE  ZEROS             TO GFCTK8-COD-SQL-ERRO
BI0810           MOVE  0001              TO GFCTK8-COD-MSG-ERRO
BI0810           MOVE '0001'             TO GFCTG2-COD-MSG
BI0810           MOVE 'S'                TO GFCTK8-FIM
BI0810           PERFORM 1310-OBTER-DESC-MSG
BI0810           MOVE  GFCTG3-DESC-MSG   TO GFCTK8-DESC-MSG-ERRO
BI0810           PERFORM 3000-FINALIZAR
BI0810        END-IF
BI0810     END-IF.

BI0412     IF GFCTK7-TIPO-ADESAO    EQUAL 'C'  OR  'A'
BI0412        IF (GFCTK7-VISUAL-COMPULS NOT EQUAL  0  AND  1 )
BI0412           MOVE  1                 TO GFCTK8-ERRO
BI0412           MOVE  ZEROS             TO GFCTK8-COD-SQL-ERRO
BI0412           MOVE  0001              TO GFCTK8-COD-MSG-ERRO
BI0412           MOVE '0001'             TO GFCTG2-COD-MSG
BI0412           MOVE 'S'                TO GFCTK8-FIM
BI0412           PERFORM 1310-OBTER-DESC-MSG
BI0412           MOVE  GFCTG3-DESC-MSG   TO GFCTK8-DESC-MSG-ERRO
BI0412           PERFORM 3000-FINALIZAR
BI0412        END-IF
BI0412     END-IF.

           IF (GFCTK7-CAGPTO           EQUAL 16  )  AND
              (GFCTK7-TIPO-ADESAO      EQUAL 'I' )
               MOVE  1                 TO GFCTK8-ERRO
               MOVE  ZEROS             TO GFCTK8-COD-SQL-ERRO
               MOVE  0904              TO GFCTK8-COD-MSG-ERRO
               MOVE '0904'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTK8-FIM
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTK8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

CREMM**    IF (GFCTK7-CAGPTO           EQUAL 16  )  AND
CREMM**       (GFCTK7-CAGENCIA         GREATER  3999 )
CREMM**        MOVE  1                 TO GFCTK8-ERRO
CREMM**        MOVE  ZEROS             TO GFCTK8-COD-SQL-ERRO
CREMM**        MOVE  0995              TO GFCTK8-COD-MSG-ERRO
CREMM**        MOVE '0995'             TO GFCTG2-COD-MSG
CREMM**        MOVE 'S'                TO GFCTK8-FIM
CREMM**        PERFORM 1310-OBTER-DESC-MSG
CREMM**        MOVE  GFCTG3-DESC-MSG   TO GFCTK8-DESC-MSG-ERRO
CREMM**        PERFORM 3000-FINALIZAR
CREMM**    END-IF.

           PERFORM 1320-SELECIONAR-GFCTB0A1.

           PERFORM 1330-VER-DATA-BOA-TARIFA.

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
           MOVE GFCTK7-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTK7-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTK7-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTK8-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0123'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 9                  TO GFCTK8-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0123'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO                 EQUAL 1
               MOVE 1                      TO GFCTK8-ERRO
               MOVE GFCTG3-COD-SQL-ERRO    TO GFCTK8-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO    TO GFCTK8-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO   TO GFCTK8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *SELECIONAR DATA ATUAL DA TABELA GFCTB0A1                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1320-SELECIONAR-GFCTB0A1        SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ATUAL,
                   DPROCM_PROX
             INTO
                   :GFCTB0A1.DPROCM-ATUAL,
                   :GFCTB0A1.DPROCM-PROX
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
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FORMATA ERROS DB2                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       1321-FORMATAR-ERRO-DB2          SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT0123'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE  SQLCA                 TO GFCT0M-SQLCA-AREA.
           MOVE  9                     TO GFCTK8-ERRO.
           MOVE  SQLCODE               TO GFCTK8-COD-SQL-ERRO
                                          GFCT0M-SQLCODE.
           MOVE SPACES                 TO GFCTK8-DESC-MSG-ERRO.

           STRING GFCTG3-DESC-MSG WRK-NOM-TAB
           DELIMITED BY '  '           INTO GFCTK8-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1321-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VER DATA BOA NA TARIFA                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1330-VER-DATA-BOA-TARIFA        SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTK7-DATA-INICIO     TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-CAD-DIA.
           MOVE WRK-MES-AUX            TO WRK-CAD-MES.
           MOVE WRK-ANO-AUX            TO WRK-CAD-ANO.

           CALL 'POOL1470'             USING WRK-CAD-DATA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTK8-ERRO
               MOVE ZEROS              TO GFCTK8-COD-SQL-ERRO
               MOVE 0672               TO GFCTK8-COD-MSG-ERRO
               MOVE '0672'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTK8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE GFCTK7-DATA-FIM        TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-CAD-DIA.
           MOVE WRK-MES-AUX            TO WRK-CAD-MES.
           MOVE WRK-ANO-AUX            TO WRK-CAD-ANO.

           CALL 'POOL1470'             USING WRK-CAD-DATA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTK8-ERRO
               MOVE ZEROS              TO GFCTK8-COD-SQL-ERRO
               MOVE 0673               TO GFCTK8-COD-MSG-ERRO
               MOVE '0673'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTK8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE GFCTK7-CPACOTE         TO CSERVC-TARIF      OF GFCTB0A2.

           INSPECT GFCTK7-DATA-INICIO  REPLACING ALL '/'    BY '.'.
           INSPECT GFCTK7-DATA-FIM     REPLACING ALL '/'    BY '.'.

           MOVE GFCTK7-DATA-INICIO     TO DINIC-VGCIA-TARIF OF GFCTB0A2
           MOVE GFCTK7-DATA-FIM        TO DFIM-VGCIA-TARIF  OF GFCTB0A2

           EXEC SQL
             SELECT  CSERVC_TARIF,
BI0412               CINDCD_CREDT_CLULR
             INTO    :GFCTB0A2.CSERVC-TARIF,
BI0412               :GFCTB0A2.CINDCD-CREDT-CLULR
             FROM   DB2PRD.PARM_SERVC_TARIF
             WHERE  CSERVC_TARIF       =  :GFCTB0A2.CSERVC-TARIF
               AND  DINIC_VGCIA_TARIF  <= :GFCTB0A2.DINIC-VGCIA-TARIF
               AND  DFIM_VGCIA_TARIF   >= :GFCTB0A2.DFIM-VGCIA-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0A2'      TO WRK-NOM-TAB
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTK8-ERRO
               MOVE ZEROS              TO GFCTK8-COD-SQL-ERRO
               MOVE 0671               TO GFCTK8-COD-MSG-ERRO
               MOVE '0671'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTK8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

BI0412     IF GFCTK7-TIPO-ADESAO    EQUAL 'C'  OR  'A'
BI0412        IF CINDCD-CREDT-CLULR OF GFCTB0A2  EQUAL  'S'
BI0412           MOVE  1                 TO GFCTK8-ERRO
BI0412           MOVE  ZEROS             TO GFCTK8-COD-SQL-ERRO
BI0412           MOVE  1688              TO GFCTK8-COD-MSG-ERRO
BI0412           MOVE '1688'             TO GFCTG2-COD-MSG
BI0412           MOVE 'S'                TO GFCTK8-FIM
BI0412           PERFORM 1310-OBTER-DESC-MSG
BI0412           MOVE  GFCTG3-DESC-MSG   TO GFCTK8-DESC-MSG-ERRO
BI0412           PERFORM 3000-FINALIZAR
BI0412        END-IF
BI0412     END-IF.

      *----------------------------------------------------------------*
       1330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAMENTO PRINCIPAL                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTK7-CAGPTO
               WHEN 15
                   PERFORM 2100-CONSISTIR-PAB
               WHEN 14
                   IF  GFCTK7-TOTAL    NOT EQUAL 'S'
                       PERFORM 2200-CONSISTIR-AGENCIA
                   END-IF
               WHEN 16
                   PERFORM 2300-CONSISTIR-EMPRESA
PW1334         WHEN 17
PW1334             IF  GFCTK7-TOTAL    NOT EQUAL 'S'
PW1334                 PERFORM 2400-CONS-POSTAL-EXPRESSO
PW1334             END-IF
PW1334         WHEN 20
PW1334             IF  GFCTK7-TOTAL    NOT EQUAL 'S'
PW1334                 PERFORM 2400-CONS-POSTAL-EXPRESSO
PW1334             END-IF
               WHEN 4
                   PERFORM 2500-CONSISTIR-CLIENTE
               WHEN 18
                   PERFORM 2700-CONSISTIR-MUNICIPIO
               WHEN 19
                   PERFORM 2800-CONSISTIR-UF
           END-EVALUATE.

           PERFORM 2900-CONSISTIR-DATA.

           PERFORM 2930-VERIFICAR-DUPLICIDADE.

           PERFORM 2950-INSERIR.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTIR PAB                                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-CONSISTIR-PAB              SECTION.
      *----------------------------------------------------------------*

           PERFORM 2200-CONSISTIR-AGENCIA.

           IF  GFCTK7-TOTAL            NOT EQUAL 'S'
               MOVE 'VRS001'           TO COMU-VERSAO
               MOVE 237                TO COMU-COD-EMPR
               MOVE GFCTK7-CAGENCIA    TO COMU-COD-DEPDC
               MOVE GFCTK7-CPAB        TO COMU-COD-POSTO
               MOVE SPACES             TO COMU-CORR-BANCARIO

               MOVE 'MESU9018'         TO WRK-MODULO

            CALL WRK-MODULO USING
                                             COMU-DADOS-9018

              IF  RETURN-CODE          NOT EQUAL ZEROS
                  IF  RETURN-CODE      EQUAL 4
                      MOVE 1           TO GFCTK8-ERRO
                      MOVE 0518        TO GFCTK8-COD-MSG-ERRO
                      MOVE '0518'      TO GFCTG2-COD-MSG
                      MOVE ZEROS       TO GFCTK8-COD-SQL-ERRO
                      PERFORM 1310-OBTER-DESC-MSG
                      MOVE GFCTG3-DESC-MSG
                                       TO GFCTK8-DESC-MSG-ERRO
                      PERFORM 3000-FINALIZAR
                  ELSE
                      IF  RETURN-CODE  EQUAL 8
                           MOVE 1      TO GFCTK8-ERRO
                           MOVE 0519   TO GFCTK8-COD-MSG-ERRO
                           MOVE '0519' TO GFCTG2-COD-MSG
                           MOVE ZEROS  TO GFCTK8-COD-SQL-ERRO
                           PERFORM 1310-OBTER-DESC-MSG
                           MOVE GFCTG3-DESC-MSG
                                       TO GFCTK8-DESC-MSG-ERRO
                           PERFORM 3000-FINALIZAR
                      ELSE
                           IF  RETURN-CODE   EQUAL 12
                               MOVE 1        TO GFCTK8-ERRO
                               MOVE 0520     TO GFCTK8-COD-MSG-ERRO
                               MOVE '0520'   TO GFCTG2-COD-MSG
                               MOVE ZEROS    TO GFCTK8-COD-SQL-ERRO
                               PERFORM 1310-OBTER-DESC-MSG
                               MOVE GFCTG3-DESC-MSG
                                             TO GFCTK8-DESC-MSG-ERRO
                               PERFORM 3000-FINALIZAR
                           END-IF
                      END-IF
                  END-IF
              ELSE
                 IF  COMU-COD-TIPO-POSTO   EQUAL 9
                     MOVE 1                TO GFCTK8-ERRO
                     MOVE 0594             TO GFCTK8-COD-MSG-ERRO
                     MOVE '0594'           TO GFCTG2-COD-MSG
                     MOVE ZEROS            TO GFCTK8-COD-SQL-ERRO
                     PERFORM 1310-OBTER-DESC-MSG
                     MOVE GFCTG3-DESC-MSG  TO GFCTK8-DESC-MSG-ERRO
                     PERFORM 3000-FINALIZAR
                 END-IF
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTIR AGENCIA                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-CONSISTIR-AGENCIA          SECTION.
      *----------------------------------------------------------------*

           MOVE 237                    TO COMU-BANCO.
           MOVE GFCTK7-CAGENCIA        TO COMU-DEPDC.
           MOVE 'GFCT0123'             TO COMU-NOME-PGM.
           MOVE 'MESU9015'             TO WRK-MODULO.
           CALL WRK-MODULO USING
                                             COMU-PARM
                                             WRK-ERRO-AREA
                                             WRK-SQLCA.

           IF  COMU-COD-RETORNO         NOT EQUAL ZEROS
               MOVE 1                   TO GFCTK8-ERRO
               MOVE 0465                TO GFCTK8-COD-MSG-ERRO
               MOVE '0465'              TO GFCTG2-COD-MSG
               MOVE ZEROS               TO GFCTK8-COD-SQL-ERRO
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG     TO GFCTK8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTIR EMPRESA                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-CONSISTIR-EMPRESA          SECTION.
      *----------------------------------------------------------------*

           PERFORM 2200-CONSISTIR-AGENCIA.

           MOVE SPACES                 TO GFCTBR-ENTRADA.
           MOVE 100                    TO GFCTBR-LL.
           MOVE ZEROS                  TO GFCTBR-ZZ.
           MOVE GFCTK7-TRANSACAO       TO GFCTBR-TRANSACAO.
           MOVE GFCTK7-FUNCAO          TO GFCTBR-FUNCAO.
           MOVE ZEROS                  TO GFCTBR-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTBR-QTDE-TOT-REG.
           MOVE GFCTK7-FUNC-BDSCO      TO GFCTBR-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTBR-FIM.
           MOVE GFCTK7-CAGENCIA        TO GFCTBR-COD-DEPDC.
           MOVE GFCTK7-CCONTA          TO GFCTBR-CCTA-CLI.
           MOVE SPACES                 TO GFCTBS-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCTBS-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE 'GFCT5060'             TO WRK-MODULO.
           CALL WRK-MODULO USING
                                             GFCTBR-ENTRADA
                                             GFCTBS-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTK8-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                           WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0123'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTBS-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTK8-FIM
               MOVE GFCTBS-ERRO        TO GFCTK8-ERRO
               MOVE GFCTBS-COD-SQL-ERRO TO GFCTK8-COD-SQL-ERRO
               MOVE GFCTBS-COD-MSG-ERRO TO GFCTK8-COD-MSG-ERRO
               STRING 'GFCT5060 - ' GFCTBS-DESC-MSG-ERRO
               DELIMITED BY SIZE       INTO GFCTK8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTIR POSTAL                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-CONS-POSTAL-EXPRESSO       SECTION.
      *----------------------------------------------------------------*

PW1334     PERFORM 2200-CONSISTIR-AGENCIA.

           MOVE 'VRS001'               TO COMU-VERSAO
           MOVE 237                    TO COMU-COD-EMPR
           MOVE GFCTK7-CAGENCIA        TO COMU-COD-DEPDC
           MOVE GFCTK7-CPAB            TO COMU-COD-POSTO
           MOVE SPACES                 TO COMU-CORR-BANCARIO

           MOVE 'MESU9018'             TO WRK-MODULO

           CALL WRK-MODULO USING
                                          COMU-DADOS-9018

           IF      RETURN-CODE         NOT EQUAL ZEROS
               IF      RETURN-CODE     EQUAL 4
                   MOVE 1              TO GFCTK8-ERRO
                   MOVE 0518           TO GFCTK8-COD-MSG-ERRO
                   MOVE '0518'         TO GFCTG2-COD-MSG
                   MOVE ZEROS          TO GFCTK8-COD-SQL-ERRO
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG
                                   TO GFCTK8-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               ELSE
                   IF      RETURN-CODE  EQUAL 8
                       MOVE 1           TO GFCTK8-ERRO
                       MOVE 0519        TO GFCTK8-COD-MSG-ERRO
                       MOVE '0519'      TO GFCTG2-COD-MSG
                       MOVE ZEROS       TO GFCTK8-COD-SQL-ERRO
                       PERFORM 1310-OBTER-DESC-MSG
                       MOVE GFCTG3-DESC-MSG
                                    TO GFCTK8-DESC-MSG-ERRO
                       PERFORM 3000-FINALIZAR
                   ELSE
                       IF      RETURN-CODE  EQUAL  12
                           MOVE 1           TO GFCTK8-ERRO
                           MOVE 0520        TO GFCTK8-COD-MSG-ERRO
                           MOVE '0520'      TO GFCTG2-COD-MSG
                           MOVE ZEROS       TO GFCTK8-COD-SQL-ERRO
                           PERFORM 1310-OBTER-DESC-MSG
                           MOVE GFCTG3-DESC-MSG
                                        TO GFCTK8-DESC-MSG-ERRO
                           PERFORM 3000-FINALIZAR
                       END-IF
                   END-IF
               END-IF
           ELSE
              IF      COMU-COD-TIPO-POSTO  NOT EQUAL 9
PW1334            IF GFCTK7-CAGPTO EQUAL 17
                      MOVE 1               TO GFCTK8-ERRO
                      MOVE 0521            TO GFCTK8-COD-MSG-ERRO
                      MOVE '0521'          TO GFCTG2-COD-MSG
                      MOVE ZEROS           TO GFCTK8-COD-SQL-ERRO
                      PERFORM 1310-OBTER-DESC-MSG
                      MOVE GFCTG3-DESC-MSG TO GFCTK8-DESC-MSG-ERRO
                      PERFORM 3000-FINALIZAR
                  ELSE
                      MOVE 1               TO GFCTK8-ERRO
                      MOVE 1480            TO GFCTK8-COD-MSG-ERRO
                      MOVE '1480'          TO GFCTG2-COD-MSG
                      MOVE ZEROS           TO GFCTK8-COD-SQL-ERRO
                      PERFORM 1310-OBTER-DESC-MSG
                      MOVE GFCTG3-DESC-MSG TO GFCTK8-DESC-MSG-ERRO
                      PERFORM 3000-FINALIZAR
                  END-IF
              END-IF
           END-IF.

PW1334     PERFORM 2450-OBTER-DADOS-YMKCB001.

      *---------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSO A TABELA YMKCB001                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2450-OBTER-DADOS-YMKCB001       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE KCIMW184-AREA-COMUNICACAO.

           MOVE 01                     TO KCIMW184-E-FUNCAO.
           MOVE GFCTK7-CAGENCIA        TO KCIMW184-E-CAG-BCRIA-1.
           MOVE GFCTK7-CPAB            TO WRK-CPAB-9-05.
           MOVE WRK-CPAB-9-03          TO KCIMW184-E-CPOSTO-BCRIO-1.
           MOVE 'KCIM0184'             TO WRK-MODULO.

           CALL WRK-MODULO             USING KCIMW184-AREA-COMUNICACAO.

           IF KCIMW184-S-COD-RETORNO  NOT EQUAL 00 AND 98
               MOVE 1                  TO GFCTK8-ERRO
               MOVE ZEROS              TO GFCTK8-COD-SQL-ERRO
               MOVE 1875               TO GFCTK8-COD-MSG-ERRO
               MOVE '1875'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTK8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.
           IF  KCIMW184-S-COD-RETORNO EQUAL 98
               IF  GFCTK7-CAGPTO       EQUAL  17
                   MOVE 1              TO GFCTK8-ERRO
                   MOVE ZEROS          TO GFCTK8-COD-SQL-ERRO
                   MOVE 0521           TO GFCTK8-COD-MSG-ERRO
                   MOVE '0521'         TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG TO GFCTK8-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               ELSE
                   MOVE 1              TO GFCTK8-ERRO
                   MOVE ZEROS          TO GFCTK8-COD-SQL-ERRO
                   MOVE 1480           TO GFCTK8-COD-MSG-ERRO
                   MOVE '1480'         TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG TO GFCTK8-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

           IF  KCIMW184-S-CEMPR-CORSP(1)   NOT EQUAL  10000  AND
               GFCTK7-CAGPTO           EQUAL  17
               MOVE 1                  TO GFCTK8-ERRO
               MOVE ZEROS              TO GFCTK8-COD-SQL-ERRO
               MOVE 0521               TO GFCTK8-COD-MSG-ERRO
               MOVE '0521'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG TO GFCTK8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  KCIMW184-S-CEMPR-CORSP(1)   EQUAL  10000  AND
               GFCTK7-CAGPTO           EQUAL  20
               MOVE 1                  TO GFCTK8-ERRO
               MOVE ZEROS              TO GFCTK8-COD-SQL-ERRO
               MOVE 1480               TO GFCTK8-COD-MSG-ERRO
               MOVE '1480'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG TO GFCTK8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *---------------------------------------------------------------*
       2450-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTIR CLIENTE                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-CONSISTIR-CLIENTE          SECTION.
      *----------------------------------------------------------------*

ST2506*    MOVE SPACES                 TO WRK-CONTROLE-0110.
ST2506*    MOVE GFCTK7-CGC-CPF         TO WRK-NUMERO-0110.
ST2506*    MOVE GFCTK7-FILIAL          TO WRK-FILIAL-0110.
ST2506*
ST2506*    CALL 'POOL0110'             USING WRK-CGC-CPF-0110
ST2506*                                      WRK-CONTROLE-0110.
ST2506     MOVE ZEROS                  TO WRK-CAD-CONTROLE. 
ST2506     MOVE GFCTK7-CGC-CPF         TO WRK-CAD-NUMERO.
ST2506     MOVE GFCTK7-FILIAL          TO WRK-CAD-FILIAL.
ST2506
ST2506     CALL WRK-BRAD2000           USING WRK-CAD-CPFCGC
ST2506                                       WRK-CAD-CONTROLE.
           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTK8-ERRO
               MOVE 0468               TO GFCTK8-COD-MSG-ERRO
               MOVE '0468'             TO GFCTG2-COD-MSG
               MOVE ZEROS              TO GFCTK8-COD-SQL-ERRO
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTK8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

ST2506*    1F  GFCTK7-CONTROLE         NOT EQUAL WRK-CONTROLE-0110
ST2506     MOVE WRK-CAD-CONTROLE       TO WRK-9-02.
ST2506     IF  GFCTK7-CONTROLE         NOT EQUAL WRK-9-X-2 
               MOVE 1                  TO GFCTK8-ERRO
               MOVE 0507               TO GFCTK8-COD-MSG-ERRO
               MOVE '0507'             TO GFCTG2-COD-MSG
               MOVE ZEROS              TO GFCTK8-COD-SQL-ERRO
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTK8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2510-PESQUISAR-BASE-CLIE.

           PERFORM 2520-VER-NO-PACOTE.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTIR CLIENTE                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2510-PESQUISAR-BASE-CLIE        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTIU-ENTRADA
                                          GFCTIV-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCTIU-ENTRADA
                      GFCTIV-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE 100                    TO GFCTIU-LL.
           MOVE ZEROS                  TO GFCTIU-ZZ.
           MOVE GFCTK7-TRANSACAO       TO GFCTIU-TRANSACAO.
           MOVE GFCTK7-FUNCAO          TO GFCTIU-FUNCAO.
           MOVE GFCTK7-FUNC-BDSCO      TO GFCTIU-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTIU-FIM.
ST2506*    MOVE GFCTK7-CGC-CPF         TO GFCTIU-NRO-CPF-CNPJ.
ST2506*    MOVE GFCTK7-FILIAL          TO GFCTIU-NRO-FILIAL.
ST2506     MOVE GFCTK7-CGC-CPF         TO GFCTIU-COD-CPF-CNPJ.
ST2506     MOVE GFCTK7-FILIAL          TO GFCTIU-COD-FILIAL.
           MOVE GFCTK7-CONTROLE        TO WRK-CONTROLE.
           MOVE WRK-CONTROLE-R         TO GFCTIU-CTRL-CPF-CNPJ.
           MOVE 'GFCT5110'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                              GFCTIU-ENTRADA
                                              GFCTIV-SAIDA
                                              GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTK8-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0123'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTIV-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTK8-FIM
               MOVE GFCTIV-ERRO        TO GFCTK8-ERRO
               MOVE GFCTIV-COD-SQL-ERRO TO GFCTK8-COD-SQL-ERRO
               MOVE GFCTIV-COD-MSG-ERRO TO GFCTK8-COD-MSG-ERRO
               STRING 'GFCT5110 - '     GFCTIV-DESC-MSG-ERRO
               DELIMITED BY SIZE       INTO GFCTK8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2520-VER-NO-PACOTE              SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTK7-CPACOTE         TO CSERVC-TARIF OF GFCTB0D8.

           EXEC SQL
             SELECT CPSSOA_SERVC_TARIF
             INTO  :GFCTB0D8.CPSSOA-SERVC-TARIF
             FROM   DB2PRD.SERVC_TARIF_PRINC
             WHERE
                    CSERVC_TARIF = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D8'      TO WRK-NOM-TAB
               MOVE 'SERVC_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0011'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF CPSSOA-SERVC-TARIF      OF GFCTB0D8 EQUAL 1 AND
              GFCTK7-FILIAL           NOT EQUAL ZEROS
              MOVE 1                  TO GFCTK8-ERRO
              MOVE ZEROS              TO GFCTK8-COD-SQL-ERRO
              MOVE 0950               TO GFCTK8-COD-MSG-ERRO
              MOVE '0950'             TO GFCTG2-COD-MSG
              PERFORM 1310-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG    TO GFCTK8-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

           IF CPSSOA-SERVC-TARIF      OF GFCTB0D8 EQUAL 2 AND
              GFCTK7-FILIAL           EQUAL ZEROS
              MOVE 1                  TO GFCTK8-ERRO
              MOVE ZEROS              TO GFCTK8-COD-SQL-ERRO
              MOVE 0951               TO GFCTK8-COD-MSG-ERRO
              MOVE '0951'             TO GFCTG2-COD-MSG
              PERFORM 1310-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG    TO GFCTK8-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      ******************************************************************
      *CONSISTIR MUNICIPIO                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2700-CONSISTIR-MUNICIPIO        SECTION.
      *----------------------------------------------------------------*

           INITIALIZE AREA-RURC88.

           MOVE GFCTK7-CMUNIC          TO COD-MUNIC-RURC88.
           MOVE 'RURC9020'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             AREA-RURC88.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTK8-ERRO
               MOVE ZEROS              TO GFCTK8-COD-SQL-ERRO
               MOVE 0732               TO GFCTK8-COD-MSG-ERRO
               MOVE '0732'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTK8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 10
               MOVE 1                  TO GFCTK8-ERRO
               MOVE ZEROS              TO GFCTK8-COD-SQL-ERRO
               MOVE 0675               TO GFCTK8-COD-MSG-ERRO
               MOVE '0675'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTK8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 20
               MOVE 1                  TO GFCTK8-ERRO
               MOVE ZEROS              TO GFCTK8-COD-SQL-ERRO
               MOVE 0733               TO GFCTK8-COD-MSG-ERRO
               MOVE '0733'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTK8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 99
               MOVE 1                  TO GFCTK8-ERRO
               MOVE SQLCODE-RURC88     TO GFCTK8-COD-SQL-ERRO
               MOVE 0734               TO GFCTK8-COD-MSG-ERRO
               MOVE '0734'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTK8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTIR UF                                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2800-CONSISTIR-UF               SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTK7-CUF             TO CSGL-UF OF DCITV002.
           MOVE DCITV002               TO WRK-AREA-DCLGEN-DCITV002.
           MOVE 'DCIT8000'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             WRK-DCIT-UNIDADE-FEDERATIVA
                                             WRK-ERRO-AREA
                                             WRK-SQLCA-DCIT.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTK8-ERRO
               MOVE ZEROS              TO GFCTK8-COD-SQL-ERRO
               MOVE 0735               TO GFCTK8-COD-MSG-ERRO
               MOVE '0735'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTK8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 1
               MOVE 1                  TO GFCTK8-ERRO
               MOVE ZEROS              TO GFCTK8-COD-SQL-ERRO
               MOVE 0736               TO GFCTK8-COD-MSG-ERRO
               MOVE '0736'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTK8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 2
               MOVE 1                  TO GFCTK8-ERRO
               MOVE ZEROS              TO GFCTK8-COD-SQL-ERRO
               MOVE 0737               TO GFCTK8-COD-MSG-ERRO
               MOVE '0737'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTK8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 3
               MOVE 1                  TO GFCTK8-ERRO
               MOVE ZEROS              TO GFCTK8-COD-SQL-ERRO
               MOVE 0738               TO GFCTK8-COD-MSG-ERRO
               MOVE '0738'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTK8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL  4
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'DCITV002'         TO WRK-NOM-TAB
               MOVE 'V01_UF'           TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTK8-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 9                  TO GFCTK8-ERRO
               MOVE 'GFCT0123'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE WRK-SQLCA-DCIT     TO SQLCA
                                          GFCT0M-SQLCA-AREA
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTK8-COD-SQL-ERRO
               MOVE SPACES             TO GFCTK8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTIR DATAS                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2900-CONSISTIR-DATA             SECTION.
      *----------------------------------------------------------------*

           STRING GFCTK7-DATA-INICIO(07:04)
                  GFCTK7-DATA-INICIO(04:02)
                  GFCTK7-DATA-INICIO(01:02)
           DELIMITED BY SIZE           INTO WRK-DATA-INICIO.

           STRING GFCTK7-DATA-FIM(07:04)
                  GFCTK7-DATA-FIM(04:02)
                  GFCTK7-DATA-FIM(01:02)
           DELIMITED BY SIZE           INTO WRK-DATA-FIM.

           STRING DPROCM-ATUAL         OF GFCTB0A1(07:04)
                  DPROCM-ATUAL         OF GFCTB0A1(04:02)
                  DPROCM-ATUAL         OF GFCTB0A1(01:02)
           DELIMITED BY SIZE           INTO WRK-DATA-PROCM.

           IF  WRK-DATA-INICIO         NOT GREATER WRK-DATA-PROCM
               MOVE 1                  TO GFCTK8-ERRO
               MOVE ZEROS              TO GFCTK8-COD-SQL-ERRO
               MOVE 0678               TO GFCTK8-COD-MSG-ERRO
               MOVE '0678'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTK8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DATA-FIM            LESS  WRK-DATA-INICIO
               MOVE 1                  TO GFCTK8-ERRO
               MOVE ZEROS              TO GFCTK8-COD-SQL-ERRO
               MOVE 0677               TO GFCTK8-COD-MSG-ERRO
               MOVE '0677'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTK8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICAR DUPLICIDADE                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2930-VERIFICAR-DUPLICIDADE      SECTION.
      *----------------------------------------------------------------*

           PERFORM 2931-ABRIR-CURSOR.

           PERFORM 2932-LER-CURSOR.

           PERFORM 2933-PROCESSAR-CURSOR UNTIL
              WRK-FIM                  EQUAL 'S' OR
              WRK-ACHOU-DUPLICIDADE    EQUAL 'S'.

           PERFORM 2934-FECHAR-CURSOR.

           IF  WRK-ACHOU-DUPLICIDADE   EQUAL 'S'
               MOVE 1                  TO GFCTK8-ERRO
               MOVE ZEROS              TO GFCTK8-COD-SQL-ERRO
               MOVE 0679               TO GFCTK8-COD-MSG-ERRO
               MOVE '0679'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTK8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2930-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRIR CURSOR                                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2931-ABRIR-CURSOR               SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM.
           MOVE 'N'                    TO WRK-ACHOU-DUPLICIDADE.
           MOVE GFCTK7-CPACOTE         TO CSERVC-TARIF     OF GFCTB0C6.
           MOVE GFCTK7-CAGPTO          TO CAGPTO-CTA       OF GFCTB0C6.
           MOVE GFCTK7-DATA-INICIO     TO DFIM-PRMSS-PCOTE OF GFCTB0C6.

260208*    MOVE GFCTK7-TIPO-ADESAO TO CINDCD-ADSAO-INDVD   OF GFCTB0C6

           EXEC SQL
               OPEN CSR01-GFCTB0C6
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C6'      TO WRK-NOM-TAB
               MOVE 'PRMSS_ADSAO_PCOTE' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2931-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LER CURSOR                                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2932-LER-CURSOR                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR01-GFCTB0C6
               INTO :GFCTB0C6.CSERVC-TARIF,
                    :GFCTB0C6.CAGPTO-CTA,
                    :GFCTB0C6.DINIC-PRMSS-PCOTE,
                    :GFCTB0C6.CSEQ-AGPTO-CTA,
BI0810              :GFCTB0C6.CINDCD-ADSAO-INDVD,
BI0412              :GFCTB0C6.CINDCD-VSLAO-COMP,
BI0810              :GFCTB0C6.CPSSOA-SERVC-TARIF
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C6'      TO WRK-NOM-TAB
               MOVE 'PRMSS_ADSAO_PCOTE' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE 'S'                 TO WRK-FIM
           END-IF.

      *----------------------------------------------------------------*
       2932-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAR CURSOR                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2933-PROCESSAR-CURSOR           SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTK7-CAGPTO
               WHEN 14
                   PERFORM 29331-VERIFICAR-AGENCIA
               WHEN 4
                   PERFORM 29332-VERIFICAR-CLIENTE
               WHEN 16
                   PERFORM 29333-VERIFICAR-EMPRESA
               WHEN 15
                   PERFORM 29334-VERIFICAR-POSTO
PW1334         WHEN 17
PW1334             PERFORM 29335-VER-POSTAL-EXPRESSO
PW1334         WHEN 20
PW1334             PERFORM 29335-VER-POSTAL-EXPRESSO
               WHEN 9
                   PERFORM 29336-VERIFICAR-SEGMENTO
               WHEN 18
                   PERFORM 29337-VERIFICAR-MUNICIPIO
               WHEN 19
                   PERFORM 29338-VERIFICAR-UF
           END-EVALUATE.

           PERFORM 2932-LER-CURSOR.

      *----------------------------------------------------------------*
       2933-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICAR AGENCIA                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       29331-VERIFICAR-AGENCIA         SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA             OF GFCTB0C6
                                       TO CAGPTO-CTA        OF GFCTB0C7.
           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0C7.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0C6
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0C7.
*********  MOVE DPROCM-PROX            OF GFCTB0A1
*********                              TO DINIC-PRMSS-PCOTE OF GFCTB0C7
           MOVE DINIC-PRMSS-PCOTE      OF GFCTB0C6
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C7
           MOVE 237                    TO CEMPR-INC         OF GFCTB0C7.

           IF  GFCTK7-TOTAL            EQUAL 'S'
MAI10          EXEC SQL
MAI10           SELECT  CEMPR_INC,
MAI10                   CDEPDC
MAI10           INTO   :GFCTB0C7.CEMPR-INC,
MAI10                  :GFCTB0C7.CDEPDC
MAI10           FROM    DB2PRD.PRMSS_GRP_DEPDC
MAI10           WHERE   CAGPTO_CTA         = :GFCTB0C7.CAGPTO-CTA
MAI10             AND   CSERVC_TARIF       = :GFCTB0C7.CSERVC-TARIF
MAI10             AND   CSEQ_AGPTO_CTA     = :GFCTB0C7.CSEQ-AGPTO-CTA
MAI10             AND   DINIC_PRMSS_PCOTE  = :GFCTB0C7.DINIC-PRMSS-PCOTE
MAI10             AND   CEMPR_INC          = :GFCTB0C7.CEMPR-INC
MAI10          END-EXEC
MAI10      ELSE
               MOVE GFCTK7-CAGENCIA    TO CDEPDC            OF GFCTB0C7

               EXEC SQL
                SELECT  CEMPR_INC,
                        CDEPDC
                INTO   :GFCTB0C7.CEMPR-INC,
                       :GFCTB0C7.CDEPDC
                FROM    DB2PRD.PRMSS_GRP_DEPDC
                WHERE   CAGPTO_CTA         = :GFCTB0C7.CAGPTO-CTA
                  AND   CSERVC_TARIF       = :GFCTB0C7.CSERVC-TARIF
                  AND   CSEQ_AGPTO_CTA     = :GFCTB0C7.CSEQ-AGPTO-CTA
                  AND   DINIC_PRMSS_PCOTE  = :GFCTB0C7.DINIC-PRMSS-PCOTE
                  AND   CEMPR_INC          = :GFCTB0C7.CEMPR-INC
MAI10             AND  (CDEPDC             = :GFCTB0C7.CDEPDC
MAI10              OR   CDEPDC             =  0)
               END-EXEC
MAI10      END-IF.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C7'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_DEPDC'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                  EQUAL ZEROS
BI0810         PERFORM     2960-VERIFICA-DUPLICADO
           END-IF.

      *----------------------------------------------------------------*
       29331-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICAR CLIENTE                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       29332-VERIFICAR-CLIENTE         SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA             OF GFCTB0C6
                                       TO CAGPTO-CTA        OF GFCTB0I0.
           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0I0.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0C6
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0I0.
********** MOVE DPROCM-PROX            OF GFCTB0A1
**********                             TO DINIC-PRMSS-PCOTE OF GFCTB0I0
           MOVE DINIC-PRMSS-PCOTE      OF GFCTB0C6
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0I0
ST2506*    MOVE GFCTK7-CGC-CPF         TO CCGC-CPF          OF GFCTB0I0.
ST2506*    MOVE GFCTK7-FILIAL          TO CFLIAL-CGC        OF GFCTB0I0.
ST2506     MOVE GFCTK7-CGC-CPF         TO CCGC-CPF-ST       OF GFCTB0I0.
ST2506     MOVE GFCTK7-FILIAL          TO CFLIAL-CGC-ST     OF GFCTB0I0.
           MOVE GFCTK7-CONTROLE        TO CCTRL-CPF-CGC     OF GFCTB0I0.

           EXEC SQL
ST2506*        SELECT  CCGC_CPF,
ST2506*                CFLIAL_CGC,
ST2506*                CCTRL_CPF_CGC
ST2506*        INTO   :GFCTB0I0.CCGC-CPF,
ST2506*               :GFCTB0I0.CFLIAL-CGC,
ST2506*               :GFCTB0I0.CCTRL-CPF-CGC
ST2506*        FROM    DB2PRD.TPRMSS_GRP_CLI
ST2506         SELECT  CCGC_CPF_ST,
ST2506                 CFLIAL_CGC_ST,
ST2506                 CCTRL_CPF_CGC_ST
ST2506         INTO   :GFCTB0I0.CCGC-CPF-ST,
ST2506                :GFCTB0I0.CFLIAL-CGC-ST,
ST2506                :GFCTB0I0.CCTRL-CPF-CGC-ST
ST2506         FROM    DB2PRD.TPRMSS_GRP_CLI
               WHERE   CAGPTO_CTA         = :GFCTB0I0.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0I0.CSERVC-TARIF
                 AND   CSEQ_AGPTO_CTA     = :GFCTB0I0.CSEQ-AGPTO-CTA
                 AND   DINIC_PRMSS_PCOTE  = :GFCTB0I0.DINIC-PRMSS-PCOTE
ST2506*          AND   CCGC_CPF           = :GFCTB0I0.CCGC-CPF
ST2506*          AND   CFLIAL_CGC         = :GFCTB0I0.CFLIAL-CGC
ST2506*          AND   CCTRL_CPF_CGC      = :GFCTB0I0.CCTRL-CPF-CGC
ST2506           AND   CCGC_CPF_ST        = :GFCTB0I0.CCGC-CPF-ST
ST2506           AND   CFLIAL_CGC_ST      = :GFCTB0I0.CFLIAL-CGC-ST
ST2506           AND   CCTRL_CPF_CGC_ST   = :GFCTB0I0.CCTRL-CPF-CGC-ST
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0I0'      TO WRK-NOM-TAB
               MOVE 'TPRMSS_GRP_CLI  '  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                  EQUAL ZEROS
BI0810         PERFORM     2960-VERIFICA-DUPLICADO
           END-IF.
      *
      *----------------------------------------------------------------*
       29332-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICAR EMPRESA                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       29333-VERIFICAR-EMPRESA         SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA             OF GFCTB0C6
                                       TO CAGPTO-CTA        OF GFCTB0C8.
           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0C8.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0C6
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0C8.
*********  MOVE DPROCM-PROX            OF GFCTB0A1
*********                              TO DINIC-PRMSS-PCOTE OF GFCTB0C8
           MOVE DINIC-PRMSS-PCOTE      OF GFCTB0C6
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C8
           MOVE GFCTK7-CAGENCIA        TO CJUNC-DEPDC-DEB   OF GFCTB0C8.
           MOVE GFCTK7-CCONTA          TO CNRO-CTA-DEB      OF GFCTB0C8.

           EXEC SQL
               SELECT  CJUNC_DEPDC_DEB,
                       CNRO_CTA_DEB
               INTO   :GFCTB0C8.CJUNC-DEPDC-DEB,
                      :GFCTB0C8.CNRO-CTA-DEB
               FROM    DB2PRD.PRMSS_GRP_EMPR
               WHERE   CAGPTO_CTA         = :GFCTB0C8.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0C8.CSERVC-TARIF
                 AND   CSEQ_AGPTO_CTA     = :GFCTB0C8.CSEQ-AGPTO-CTA
                 AND   DINIC_PRMSS_PCOTE  = :GFCTB0C8.DINIC-PRMSS-PCOTE
                 AND   CJUNC_DEPDC_DEB    = :GFCTB0C8.CJUNC-DEPDC-DEB
                 AND   CNRO_CTA_DEB       = :GFCTB0C8.CNRO-CTA-DEB
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C8'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_EMPR'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                  EQUAL ZEROS
BI0810         PERFORM     2960-VERIFICA-DUPLICADO
           END-IF.

      *----------------------------------------------------------------*
       29333-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICAR POSTO                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       29334-VERIFICAR-POSTO           SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA             OF GFCTB0C6
                                       TO CAGPTO-CTA        OF GFCTB0D0.
           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D0.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0C6
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0D0.
*********  MOVE DPROCM-PROX            OF GFCTB0A1
*********                              TO DINIC-PRMSS-PCOTE OF GFCTB0D0
           MOVE DINIC-PRMSS-PCOTE      OF GFCTB0C6
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0D0
           MOVE 237                    TO CEMPR-INC         OF GFCTB0D0.
           MOVE GFCTK7-CAGENCIA        TO CDEPDC            OF GFCTB0D0.

MAI10      IF  GFCTK7-TOTAL            EQUAL 'S'
MAI10          EXEC SQL
MAI10           SELECT  CEMPR_INC,
MAI10                   CDEPDC,
MAI10                   CPOSTO_SERVC
MAI10           INTO   :GFCTB0D0.CEMPR-INC,
MAI10                  :GFCTB0D0.CDEPDC,
MAI10                  :GFCTB0D0.CPOSTO-SERVC
MAI10           FROM    DB2PRD.PRMSS_GRP_PAB
MAI10           WHERE   CAGPTO_CTA         = :GFCTB0D0.CAGPTO-CTA
MAI10             AND   CSERVC_TARIF       = :GFCTB0D0.CSERVC-TARIF
MAI10             AND   CSEQ_AGPTO_CTA     = :GFCTB0D0.CSEQ-AGPTO-CTA
MAI10             AND   DINIC_PRMSS_PCOTE  = :GFCTB0D0.DINIC-PRMSS-PCOTE
MAI10             AND   CEMPR_INC          = :GFCTB0D0.CEMPR-INC
MAI10             AND   CDEPDC             = :GFCTB0D0.CDEPDC
MAI10          END-EXEC
MAI10      ELSE
               MOVE GFCTK7-CPAB        TO CPOSTO-SERVC      OF GFCTB0D0

               EXEC SQL
                SELECT  CEMPR_INC,
                        CDEPDC,
                        CPOSTO_SERVC
                INTO   :GFCTB0D0.CEMPR-INC,
                       :GFCTB0D0.CDEPDC,
                       :GFCTB0D0.CPOSTO-SERVC
                FROM    DB2PRD.PRMSS_GRP_PAB
                WHERE   CAGPTO_CTA         = :GFCTB0D0.CAGPTO-CTA
                  AND   CSERVC_TARIF       = :GFCTB0D0.CSERVC-TARIF
                  AND   CSEQ_AGPTO_CTA     = :GFCTB0D0.CSEQ-AGPTO-CTA
                  AND   DINIC_PRMSS_PCOTE  = :GFCTB0D0.DINIC-PRMSS-PCOTE
                  AND   CEMPR_INC          = :GFCTB0D0.CEMPR-INC
                  AND   CDEPDC             = :GFCTB0D0.CDEPDC
MAI10             AND  (CPOSTO_SERVC       = :GFCTB0D0.CPOSTO-SERVC
MAI10             OR    CPOSTO_SERVC       =  0 )
               END-EXEC
MAI10      END-IF.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D0'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_PAB'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0090'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                  EQUAL ZEROS
BI0810         PERFORM     2960-VERIFICA-DUPLICADO
           END-IF.

      *----------------------------------------------------------------*
       29334-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICAR POSTAL                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       29335-VER-POSTAL-EXPRESSO       SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA             OF GFCTB0C6
                                       TO CAGPTO-CTA        OF GFCTB0D1.
           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D1.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0C6
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0D1.
********** MOVE DPROCM-PROX            OF GFCTB0A1
**********                             TO DINIC-PRMSS-PCOTE OF GFCTB0D1
           MOVE DINIC-PRMSS-PCOTE      OF GFCTB0C6
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0D1
           MOVE 237                    TO CEMPR-INC         OF GFCTB0D1.

MAI10      IF  GFCTK7-TOTAL            EQUAL 'S'
MAI10          EXEC SQL
MAI10           SELECT  CEMPR_INC,
MAI10                   CDEPDC,
MAI10                   CPOSTO_SERVC
MAI10           INTO   :GFCTB0D1.CEMPR-INC,
MAI10                  :GFCTB0D1.CDEPDC,
MAI10                  :GFCTB0D1.CPOSTO-SERVC
MAI10           FROM    DB2PRD.PRMSS_GRP_PSTAL
MAI10           WHERE   CAGPTO_CTA         = :GFCTB0D1.CAGPTO-CTA
MAI10             AND   CSERVC_TARIF       = :GFCTB0D1.CSERVC-TARIF
MAI10             AND   CSEQ_AGPTO_CTA     = :GFCTB0D1.CSEQ-AGPTO-CTA
MAI10             AND   DINIC_PRMSS_PCOTE  = :GFCTB0D1.DINIC-PRMSS-PCOTE
MAI10             AND   CEMPR_INC          = :GFCTB0D1.CEMPR-INC
MAI10          END-EXEC
           ELSE
               MOVE GFCTK7-CAGENCIA    TO CDEPDC            OF GFCTB0D1
               MOVE GFCTK7-CPAB        TO CPOSTO-SERVC      OF GFCTB0D1

               EXEC SQL
                SELECT  CEMPR_INC,
                        CDEPDC,
                        CPOSTO_SERVC
                INTO   :GFCTB0D1.CEMPR-INC,
                       :GFCTB0D1.CDEPDC,
                       :GFCTB0D1.CPOSTO-SERVC
                FROM    DB2PRD.PRMSS_GRP_PSTAL
                WHERE   CAGPTO_CTA         = :GFCTB0D1.CAGPTO-CTA
                  AND   CSERVC_TARIF       = :GFCTB0D1.CSERVC-TARIF
                  AND   CSEQ_AGPTO_CTA     = :GFCTB0D1.CSEQ-AGPTO-CTA
                  AND   DINIC_PRMSS_PCOTE  = :GFCTB0D1.DINIC-PRMSS-PCOTE
                  AND   CEMPR_INC          = :GFCTB0D1.CEMPR-INC
MAI10             AND  (CDEPDC             = :GFCTB0D1.CDEPDC
MAI10              OR   CDEPDC             =  0 )
MAI10             AND  (CPOSTO_SERVC       = :GFCTB0D1.CPOSTO-SERVC
MAI10              OR   CPOSTO_SERVC       =  0 )
               END-EXEC
MAI10      END-IF.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D1'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_PSTAL'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0100'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                  EQUAL ZEROS
BI0810         PERFORM     2960-VERIFICA-DUPLICADO
           END-IF.

      *----------------------------------------------------------------*
       29335-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICAR SEGMENTO                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       29336-VERIFICAR-SEGMENTO        SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA             OF GFCTB0C6
                                       TO CAGPTO-CTA        OF GFCTB0D2.
           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D2.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0C6
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0D2.
********   MOVE DPROCM-PROX            OF GFCTB0A1 TO
*********                                 DINIC-PRMSS-PCOTE OF GFCTB0D2.
           MOVE DINIC-PRMSS-PCOTE      OF GFCTB0C6 TO
                                          DINIC-PRMSS-PCOTE OF GFCTB0D2.
           MOVE GFCTK7-CSEGMENTO       TO CSGMTO-GSTAO-TARIF
                                                            OF GFCTB0D2.

           EXEC SQL
               SELECT  CSGMTO_GSTAO_TARIF
               INTO   :GFCTB0D2.CSGMTO-GSTAO-TARIF
               FROM    DB2PRD.PRMSS_GRP_SGMTO
               WHERE   CAGPTO_CTA         = :GFCTB0D2.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0D2.CSERVC-TARIF
                 AND   CSEQ_AGPTO_CTA     = :GFCTB0D2.CSEQ-AGPTO-CTA
                 AND   DINIC_PRMSS_PCOTE  = :GFCTB0D2.DINIC-PRMSS-PCOTE
                 AND   CSGMTO_GSTAO_TARIF = :GFCTB0D2.CSGMTO-GSTAO-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D2'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_SGMTO'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0110'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                  EQUAL ZEROS
BI0810         PERFORM     2960-VERIFICA-DUPLICADO
           END-IF.

      *----------------------------------------------------------------*
       29336-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICAR MUNICIPIO                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       29337-VERIFICAR-MUNICIPIO       SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA             OF GFCTB0C6
                                       TO CAGPTO-CTA        OF GFCTB0C9.
           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0C9.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0C6
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0C9.
********   MOVE DPROCM-PROX            OF GFCTB0A1
********                               TO DINIC-PRMSS-PCOTE OF GFCTB0C9
           MOVE DINIC-PRMSS-PCOTE      OF GFCTB0C6
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C9
           MOVE GFCTK7-CMUNIC          TO CMUN-IBGE         OF GFCTB0C9.

           EXEC SQL
               SELECT  CMUN_IBGE
               INTO   :GFCTB0C9.CMUN-IBGE
               FROM    DB2PRD.PRMSS_GRP_MUN
               WHERE   CAGPTO_CTA         = :GFCTB0C9.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0C9.CSERVC-TARIF
                 AND   CSEQ_AGPTO_CTA     = :GFCTB0C9.CSEQ-AGPTO-CTA
                 AND   DINIC_PRMSS_PCOTE  = :GFCTB0C9.DINIC-PRMSS-PCOTE
                 AND   CMUN_IBGE          = :GFCTB0C9.CMUN-IBGE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C9'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_MUN'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0120'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                  EQUAL ZEROS
BI0810         PERFORM     2960-VERIFICA-DUPLICADO
           END-IF.

      *----------------------------------------------------------------*
       29337-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICAR UF                                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       29338-VERIFICAR-UF              SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA             OF GFCTB0C6
                                       TO CAGPTO-CTA        OF GFCTB0D3.
           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D3.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0C6
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0D3.
*********  MOVE DPROCM-PROX            OF GFCTB0A1
*********                              TO DINIC-PRMSS-PCOTE OF GFCTB0D3
           MOVE DINIC-PRMSS-PCOTE      OF GFCTB0C6
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0D3
           MOVE GFCTK7-CUF             TO CSGL-UF           OF GFCTB0D3.

           EXEC SQL
             SELECT
                   CSGL_UF
             INTO
                   :GFCTB0D3.CSGL-UF
             FROM   DB2PRD.PRMSS_GRP_UF
             WHERE
                   CAGPTO_CTA         = :GFCTB0D3.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0D3.CSERVC-TARIF       AND
                   CSEQ_AGPTO_CTA     = :GFCTB0D3.CSEQ-AGPTO-CTA     AND
                   DINIC_PRMSS_PCOTE  = :GFCTB0D3.DINIC-PRMSS-PCOTE  AND
                   CSGL_UF            = :GFCTB0D3.CSGL-UF
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D3'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_UF'     TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0130'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                  EQUAL ZEROS
BI0810         PERFORM     2960-VERIFICA-DUPLICADO
           END-IF.

      *----------------------------------------------------------------*
       29338-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHAR CURSOR                                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2934-FECHAR-CURSOR              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-GFCTB0C6
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C6'      TO WRK-NOM-TAB
               MOVE 'PRMSS_ADSAO_PCOTE' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0140'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2934-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *INSERIR                                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2950-INSERIR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 29510-VE-FUTURO.

           PERFORM 2951-INSERIR-C6.

           EVALUATE GFCTK7-CAGPTO
               WHEN 15
                   PERFORM 2952-GERAR-PAB
               WHEN 14
                   PERFORM 2953-GERAR-AGENCIA
               WHEN 16
                   PERFORM 2954-GERAR-EMPRESA
PW1334         WHEN 17
PW1334             PERFORM 2955-GERAR-POSTAL-EXPRE
PW1334         WHEN 20
PW1334             PERFORM 2955-GERAR-POSTAL-EXPRE
               WHEN 4
                   PERFORM 2956-GERAR-CLIENTE
               WHEN 9
                   PERFORM 2957-GERAR-SEGMENTO
               WHEN 18
                   PERFORM 2958-GERAR-MUNICIPIO
               WHEN 19
                   PERFORM 2959-GERAR-UF
           END-EVALUATE.

      *----------------------------------------------------------------*
       2950-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       29510-VE-FUTURO                 SECTION.
      *----------------------------------------------------------------*

           PERFORM 29511-ABRIR-CURSOR.

           PERFORM 29512-LER-CURSOR.

           PERFORM 29513-PROCESSAR-CURSOR UNTIL
              WRK-FIM                  EQUAL 'S' OR
              WRK-ACHOU-FUTURO         EQUAL 'S'.

           PERFORM 29514-FECHAR-CURSOR.

           IF  WRK-ACHOU-FUTURO        EQUAL 'S'
               MOVE  1                 TO GFCTK8-ERRO
               MOVE  ZEROS             TO GFCTK8-COD-SQL-ERRO
               MOVE  0948              TO GFCTK8-COD-MSG-ERRO
               MOVE '0948'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTK8-FIM
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTK8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2930-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       29511-ABRIR-CURSOR              SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM.
           MOVE 'N'                    TO WRK-ACHOU-FUTURO.
           MOVE GFCTK7-CPACOTE         TO CSERVC-TARIF     OF GFCTB0C6.
           MOVE GFCTK7-CAGPTO          TO CAGPTO-CTA       OF GFCTB0C6.
           MOVE DPROCM-ATUAL OF GFCTB0A1
                                      TO DINIC-PRMSS-PCOTE OF GFCTB0C6.

250208*    MOVE GFCTK7-TIPO-ADESAO
250208*                            TO CINDCD-ADSAO-INDVD   OF GFCTB0C6

           EXEC SQL
               OPEN CSR02-GFCTB0C6
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C6'      TO WRK-NOM-TAB
               MOVE 'PRMSS_ADSAO_PCOTE' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0150'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       29511-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       29512-LER-CURSOR                SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR02-GFCTB0C6
               INTO :GFCTB0C6.CSERVC-TARIF,
                    :GFCTB0C6.CAGPTO-CTA,
                    :GFCTB0C6.DINIC-PRMSS-PCOTE,
                    :GFCTB0C6.CSEQ-AGPTO-CTA,
BI0810              :GFCTB0C6.CINDCD-ADSAO-INDVD,
BI0412              :GFCTB0C6.CINDCD-VSLAO-COMP,
BI0810              :GFCTB0C6.CPSSOA-SERVC-TARIF
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C6'      TO WRK-NOM-TAB
               MOVE 'PRMSS_ADSAO_PCOTE' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0160'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE 'S'                 TO WRK-FIM
           END-IF.

      *----------------------------------------------------------------*
       29512-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       29513-PROCESSAR-CURSOR          SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTK7-CAGPTO
               WHEN 14
                   PERFORM 295131-VERIFICAR-AGENCIA
               WHEN 4
                   PERFORM 295132-VERIFICAR-CLIENTE
               WHEN 16
                   PERFORM 295133-VERIFICAR-EMPRESA
               WHEN 15
                   PERFORM 295134-VERIFICAR-POSTO
PW1334         WHEN 17
PW1334             PERFORM 295135-VER-POSTAL-EXPRESSO
PW1334         WHEN 20
PW1334             PERFORM 295135-VER-POSTAL-EXPRESSO
               WHEN 9
                   PERFORM 295136-VERIFICAR-SEGMENTO
               WHEN 18
                   PERFORM 295137-VERIFICAR-MUNICIPIO
               WHEN 19
                   PERFORM 295138-VERIFICAR-UF
           END-EVALUATE.

           PERFORM 29512-LER-CURSOR.

      *----------------------------------------------------------------*
       29513-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       295131-VERIFICAR-AGENCIA        SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA             OF GFCTB0C6
                                       TO CAGPTO-CTA        OF GFCTB0C7.
           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0C7.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0C6
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0C7.
           MOVE DINIC-PRMSS-PCOTE      OF GFCTB0C6
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C7.
           MOVE 237                    TO CEMPR-INC         OF GFCTB0C7.

MAI10      IF  GFCTK7-TOTAL            EQUAL 'S'
MAI10          EXEC SQL
MAI10           SELECT  CEMPR_INC,
MAI10                   CDEPDC
MAI10           INTO   :GFCTB0C7.CEMPR-INC,
MAI10                  :GFCTB0C7.CDEPDC
MAI10           FROM    DB2PRD.PRMSS_GRP_DEPDC
MAI10           WHERE   CAGPTO_CTA         = :GFCTB0C7.CAGPTO-CTA
MAI10             AND   CSERVC_TARIF       = :GFCTB0C7.CSERVC-TARIF
MAI10             AND   CSEQ_AGPTO_CTA     = :GFCTB0C7.CSEQ-AGPTO-CTA
MAI10             AND   DINIC_PRMSS_PCOTE  = :GFCTB0C7.DINIC-PRMSS-PCOTE
MAI10             AND   CEMPR_INC          = :GFCTB0C7.CEMPR-INC
MAI10          END-EXEC
           ELSE
               MOVE GFCTK7-CAGENCIA    TO CDEPDC            OF GFCTB0C7

               EXEC SQL
                SELECT  CEMPR_INC,
                        CDEPDC
                INTO   :GFCTB0C7.CEMPR-INC,
                       :GFCTB0C7.CDEPDC
                FROM    DB2PRD.PRMSS_GRP_DEPDC
                WHERE   CAGPTO_CTA         = :GFCTB0C7.CAGPTO-CTA
                  AND   CSERVC_TARIF       = :GFCTB0C7.CSERVC-TARIF
                  AND   CSEQ_AGPTO_CTA     = :GFCTB0C7.CSEQ-AGPTO-CTA
                  AND   DINIC_PRMSS_PCOTE  = :GFCTB0C7.DINIC-PRMSS-PCOTE
                  AND   CEMPR_INC          = :GFCTB0C7.CEMPR-INC
MAI10             AND  (CDEPDC             = :GFCTB0C7.CDEPDC
MAI10              OR   CDEPDC             =  0 )
               END-EXEC
MAI10      END-IF.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C7'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_DEPDC'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0170'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                  EQUAL ZEROS
BI0810         PERFORM    2970-VERIFICA-FUTURO
           END-IF.

      *----------------------------------------------------------------*
       295131-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       295132-VERIFICAR-CLIENTE        SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA             OF GFCTB0C6
                                       TO CAGPTO-CTA        OF GFCTB0I0.
           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0I0.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0C6
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0I0.
           MOVE DINIC-PRMSS-PCOTE      OF GFCTB0C6
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0I0.
           MOVE GFCTK7-CGC-CPF         TO CCGC-CPF          OF GFCTB0I0.
           MOVE GFCTK7-FILIAL          TO CFLIAL-CGC        OF GFCTB0I0.
           MOVE GFCTK7-CONTROLE        TO CCTRL-CPF-CGC     OF GFCTB0I0.

           EXEC SQL
ST2506*        SELECT  CCGC_CPF,
ST2506*                CFLIAL_CGC,
ST2506*                CCTRL_CPF_CGC
ST2506*        INTO   :GFCTB0I0.CCGC-CPF,
ST2506*               :GFCTB0I0.CFLIAL-CGC,
ST2506*               :GFCTB0I0.CCTRL-CPF-CGC
ST2506         SELECT  CCGC_CPF_ST,
ST2506                 CFLIAL_CGC_ST,
ST2506                 CCTRL_CPF_CGC_ST
ST2506         INTO   :GFCTB0I0.CCGC-CPF-ST,
ST2506                :GFCTB0I0.CFLIAL-CGC-ST,
ST2506                :GFCTB0I0.CCTRL-CPF-CGC-ST
               FROM    DB2PRD.TPRMSS_GRP_CLI
               WHERE   CAGPTO_CTA         = :GFCTB0I0.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0I0.CSERVC-TARIF
                 AND   CSEQ_AGPTO_CTA     = :GFCTB0I0.CSEQ-AGPTO-CTA
                 AND   DINIC_PRMSS_PCOTE  = :GFCTB0I0.DINIC-PRMSS-PCOTE
ST2506*          AND   CCGC_CPF           = :GFCTB0I0.CCGC-CPF
ST2506*          AND   CFLIAL_CGC         = :GFCTB0I0.CFLIAL-CGC
ST2506*          AND   CCTRL_CPF_CGC      = :GFCTB0I0.CCTRL-CPF-CGC
ST2506           AND   CCGC_CPF_ST        = :GFCTB0I0.CCGC-CPF-ST
ST2506           AND   CFLIAL_CGC_ST      = :GFCTB0I0.CFLIAL-CGC-ST
ST2506           AND   CCTRL_CPF_CGC_ST   = :GFCTB0I0.CCTRL-CPF-CGC-ST
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0I0'      TO WRK-NOM-TAB
               MOVE 'TPRMSS_GRP_CLI  '  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0180'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                  EQUAL ZEROS
BI0810         PERFORM    2970-VERIFICA-FUTURO
           END-IF.

      *----------------------------------------------------------------*
       295132-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       295133-VERIFICAR-EMPRESA        SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA             OF GFCTB0C6
                                       TO CAGPTO-CTA        OF GFCTB0C8.
           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0C8.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0C6
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0C8.
           MOVE DINIC-PRMSS-PCOTE      OF GFCTB0C6
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C8.
           MOVE GFCTK7-CAGENCIA        TO CJUNC-DEPDC-DEB   OF GFCTB0C8.
           MOVE GFCTK7-CCONTA          TO CNRO-CTA-DEB      OF GFCTB0C8.

           EXEC SQL
               SELECT  CJUNC_DEPDC_DEB,
                       CNRO_CTA_DEB
               INTO   :GFCTB0C8.CJUNC-DEPDC-DEB,
                      :GFCTB0C8.CNRO-CTA-DEB
               FROM    DB2PRD.PRMSS_GRP_EMPR
               WHERE   CAGPTO_CTA         = :GFCTB0C8.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0C8.CSERVC-TARIF
                 AND   CSEQ_AGPTO_CTA     = :GFCTB0C8.CSEQ-AGPTO-CTA
                 AND   DINIC_PRMSS_PCOTE  = :GFCTB0C8.DINIC-PRMSS-PCOTE
                 AND   CJUNC_DEPDC_DEB    = :GFCTB0C8.CJUNC-DEPDC-DEB
                 AND   CNRO_CTA_DEB       = :GFCTB0C8.CNRO-CTA-DEB
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C8'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_EMPR'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0190'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                  EQUAL ZEROS
BI0810         PERFORM    2970-VERIFICA-FUTURO
           END-IF.

      *----------------------------------------------------------------*
       295133-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       295134-VERIFICAR-POSTO           SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA             OF GFCTB0C6
                                       TO CAGPTO-CTA        OF GFCTB0D0.
           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D0.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0C6
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0D0.
           MOVE DINIC-PRMSS-PCOTE      OF GFCTB0C6
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0D0.
           MOVE 237                    TO CEMPR-INC         OF GFCTB0D0.
           MOVE GFCTK7-CAGENCIA        TO CDEPDC            OF GFCTB0D0.

           IF  GFCTK7-TOTAL            EQUAL 'S'
MAI10          EXEC SQL
MAI10           SELECT  CEMPR_INC,
MAI10                   CDEPDC,
MAI10                   CPOSTO_SERVC
MAI10           INTO   :GFCTB0D0.CEMPR-INC,
MAI10                  :GFCTB0D0.CDEPDC,
MAI10                  :GFCTB0D0.CPOSTO-SERVC
MAI10           FROM    DB2PRD.PRMSS_GRP_PAB
MAI10           WHERE   CAGPTO_CTA         = :GFCTB0D0.CAGPTO-CTA
MAI10             AND   CSERVC_TARIF       = :GFCTB0D0.CSERVC-TARIF
MAI10             AND   CSEQ_AGPTO_CTA     = :GFCTB0D0.CSEQ-AGPTO-CTA
MAI10             AND   DINIC_PRMSS_PCOTE  = :GFCTB0D0.DINIC-PRMSS-PCOTE
MAI10             AND   CEMPR_INC          = :GFCTB0D0.CEMPR-INC
MAI10             AND   CDEPDC             = :GFCTB0D0.CDEPDC
MAI10           END-EXEC
           ELSE
               MOVE GFCTK7-CPAB        TO CPOSTO-SERVC      OF GFCTB0D0

               EXEC SQL
                SELECT  CEMPR_INC,
                        CDEPDC,
                        CPOSTO_SERVC
                INTO   :GFCTB0D0.CEMPR-INC,
                       :GFCTB0D0.CDEPDC,
                       :GFCTB0D0.CPOSTO-SERVC
                FROM    DB2PRD.PRMSS_GRP_PAB
                WHERE   CAGPTO_CTA         = :GFCTB0D0.CAGPTO-CTA
                  AND   CSERVC_TARIF       = :GFCTB0D0.CSERVC-TARIF
                  AND   CSEQ_AGPTO_CTA     = :GFCTB0D0.CSEQ-AGPTO-CTA
                  AND   DINIC_PRMSS_PCOTE  = :GFCTB0D0.DINIC-PRMSS-PCOTE
                  AND   CEMPR_INC          = :GFCTB0D0.CEMPR-INC
                  AND   CDEPDC             = :GFCTB0D0.CDEPDC
MAI10             AND  (CPOSTO_SERVC       = :GFCTB0D0.CPOSTO-SERVC
MAI10              OR   CPOSTO_SERVC       =  0 )
                END-EXEC
MAI10      END-IF.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D0'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_PAB'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0200'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                  EQUAL ZEROS
BI0810         PERFORM    2970-VERIFICA-FUTURO
           END-IF.

      *----------------------------------------------------------------*
       295134-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       295135-VER-POSTAL-EXPRESSO       SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA             OF GFCTB0C6
                                       TO CAGPTO-CTA        OF GFCTB0D1.
           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D1.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0C6
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0D1.
           MOVE DINIC-PRMSS-PCOTE      OF GFCTB0C6
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0D1
           MOVE 237                    TO CEMPR-INC         OF GFCTB0D1.

           IF  GFCTK7-TOTAL            EQUAL 'S'
MAI10          EXEC SQL
MAI10           SELECT  CEMPR_INC,
MAI10                   CDEPDC,
MAI10                   CPOSTO_SERVC
MAI10           INTO   :GFCTB0D1.CEMPR-INC,
MAI10                  :GFCTB0D1.CDEPDC,
MAI10                  :GFCTB0D1.CPOSTO-SERVC
MAI10           FROM    DB2PRD.PRMSS_GRP_PSTAL
MAI10           WHERE   CAGPTO_CTA         = :GFCTB0D1.CAGPTO-CTA
MAI10             AND   CSERVC_TARIF       = :GFCTB0D1.CSERVC-TARIF
MAI10             AND   CSEQ_AGPTO_CTA     = :GFCTB0D1.CSEQ-AGPTO-CTA
MAI10             AND   DINIC_PRMSS_PCOTE  = :GFCTB0D1.DINIC-PRMSS-PCOTE
MAI10             AND   CEMPR_INC          = :GFCTB0D1.CEMPR-INC
MAI10          END-EXEC
           ELSE
               MOVE GFCTK7-CAGENCIA    TO CDEPDC            OF GFCTB0D1
               MOVE GFCTK7-CPAB        TO CPOSTO-SERVC      OF GFCTB0D1

               EXEC SQL
                SELECT  CEMPR_INC,
                        CDEPDC,
                        CPOSTO_SERVC
                INTO   :GFCTB0D1.CEMPR-INC,
                       :GFCTB0D1.CDEPDC,
                       :GFCTB0D1.CPOSTO-SERVC
                FROM    DB2PRD.PRMSS_GRP_PSTAL
                WHERE   CAGPTO_CTA         = :GFCTB0D1.CAGPTO-CTA
                  AND   CSERVC_TARIF       = :GFCTB0D1.CSERVC-TARIF
                  AND   CSEQ_AGPTO_CTA     = :GFCTB0D1.CSEQ-AGPTO-CTA
                  AND   DINIC_PRMSS_PCOTE  = :GFCTB0D1.DINIC-PRMSS-PCOTE
                  AND   CEMPR_INC          = :GFCTB0D1.CEMPR-INC
MAI10             AND  (CDEPDC             = :GFCTB0D1.CDEPDC
MAI10              OR   CDEPDC             =  0 )
MAI10             AND  (CPOSTO_SERVC       = :GFCTB0D1.CPOSTO-SERVC
MAI10              OR   CPOSTO_SERVC       =  0 )
               END-EXEC
MAI10      END-IF.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D1'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_PSTAL'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0210'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                  EQUAL ZEROS
BI0810         PERFORM    2970-VERIFICA-FUTURO
           END-IF.

      *----------------------------------------------------------------*
       295135-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       295136-VERIFICAR-SEGMENTO        SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA             OF GFCTB0C6
                                       TO CAGPTO-CTA        OF GFCTB0D2.
           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D2.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0C6
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0D2.
           MOVE DINIC-PRMSS-PCOTE      OF GFCTB0C6
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0D2.
           MOVE GFCTK7-CSEGMENTO       TO CSGMTO-GSTAO-TARIF
                                                            OF GFCTB0D2.

           EXEC SQL
               SELECT  CSGMTO_GSTAO_TARIF
               INTO   :GFCTB0D2.CSGMTO-GSTAO-TARIF
               FROM    DB2PRD.PRMSS_GRP_SGMTO
               WHERE   CAGPTO_CTA         = :GFCTB0D2.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0D2.CSERVC-TARIF
                 AND   CSEQ_AGPTO_CTA     = :GFCTB0D2.CSEQ-AGPTO-CTA
                 AND   DINIC_PRMSS_PCOTE  = :GFCTB0D2.DINIC-PRMSS-PCOTE
                 AND   CSGMTO_GSTAO_TARIF = :GFCTB0D2.CSGMTO-GSTAO-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D2'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_SGMTO'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0220'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                  EQUAL ZEROS
BI0810         PERFORM    2970-VERIFICA-FUTURO
           END-IF.

      *----------------------------------------------------------------*
       295136-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       295137-VERIFICAR-MUNICIPIO       SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA             OF GFCTB0C6
                                       TO CAGPTO-CTA        OF GFCTB0C9.
           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0C9.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0C6
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0C9.
           MOVE DINIC-PRMSS-PCOTE      OF GFCTB0C6
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C9.
           MOVE GFCTK7-CMUNIC          TO CMUN-IBGE         OF GFCTB0C9.

           EXEC SQL
               SELECT  CMUN_IBGE
               INTO   :GFCTB0C9.CMUN-IBGE
               FROM    DB2PRD.PRMSS_GRP_MUN
               WHERE   CAGPTO_CTA         = :GFCTB0C9.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0C9.CSERVC-TARIF
                 AND   CSEQ_AGPTO_CTA     = :GFCTB0C9.CSEQ-AGPTO-CTA
                 AND   DINIC_PRMSS_PCOTE  = :GFCTB0C9.DINIC-PRMSS-PCOTE
                 AND   CMUN_IBGE          = :GFCTB0C9.CMUN-IBGE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C9'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_MUN'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0230'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                  EQUAL ZEROS
BI0810         PERFORM    2970-VERIFICA-FUTURO
           END-IF.

      *----------------------------------------------------------------*
       295137-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       295138-VERIFICAR-UF              SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA             OF GFCTB0C6
                                       TO CAGPTO-CTA        OF GFCTB0D3.
           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D3.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0C6
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0D3.
           MOVE DINIC-PRMSS-PCOTE      OF GFCTB0C6
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0D3.
           MOVE GFCTK7-CUF             TO CSGL-UF           OF GFCTB0D3.

           EXEC SQL
             SELECT
                   CSGL_UF
             INTO
                   :GFCTB0D3.CSGL-UF
             FROM   DB2PRD.PRMSS_GRP_UF
             WHERE
                   CAGPTO_CTA         = :GFCTB0D3.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0D3.CSERVC-TARIF       AND
                   CSEQ_AGPTO_CTA     = :GFCTB0D3.CSEQ-AGPTO-CTA     AND
                   DINIC_PRMSS_PCOTE  = :GFCTB0D3.DINIC-PRMSS-PCOTE  AND
                   CSGL_UF            = :GFCTB0D3.CSGL-UF
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D3'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_UF'     TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0240'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                  EQUAL ZEROS
BI0810         PERFORM    2970-VERIFICA-FUTURO
           END-IF.

      *----------------------------------------------------------------*
       295138-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       29514-FECHAR-CURSOR              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR02-GFCTB0C6
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C6'      TO WRK-NOM-TAB
               MOVE 'PRMSS_ADSAO_PCOTE' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0250'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2934-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *INSERIR NA TABELA GFCTB0C6                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2951-INSERIR-C6                 SECTION.
      *----------------------------------------------------------------*

           PERFORM 29511-OBTER-MAIOR-SEQ.

           MOVE GFCTK7-CPACOTE         TO CSERVC-TARIF      OF GFCTB0C6.
           MOVE GFCTK7-CAGPTO          TO CAGPTO-CTA        OF GFCTB0C6.
           MOVE WRK-SEQUENCIA          TO CSEQ-AGPTO-CTA    OF GFCTB0C6.
           MOVE GFCTK7-DATA-INICIO     TO DINIC-PRMSS-PCOTE OF GFCTB0C6.
           MOVE GFCTK7-FUNC-BDSCO      TO WRK-FUNC-BDSCO.
           MOVE WRK-FUNC-BDSCO-R       TO CFUNC-MANUT-INCL  OF GFCTB0C6.
           MOVE ZEROS                  TO CFUNC-MANUT       OF GFCTB0C6.
           MOVE GFCTK7-DATA-FIM        TO DFIM-PRMSS-PCOTE  OF GFCTB0C6.

SMS        MOVE GFCTK7-TIPO-ADESAO     TO CINDCD-ADSAO-INDVD
SMS                                                         OF GFCTB0C6
SMS
SMS        IF  GFCTK7-TIPO-ADESAO      EQUAL 'C'
SMS            IF GFCTK7-SOLICITA-EXCECAO   EQUAL '1'
SMS                MOVE 'S'            TO CINDCD-EXCEC-ADSAO
SMS                                                         OF GFCTB0C6
SMS            ELSE
SMS                MOVE ' '            TO CINDCD-EXCEC-ADSAO
SMS                                                         OF GFCTB0C6
SMS            END-IF
SMS            MOVE ZEROS              TO CIDTFD-PRMSS-INDVD
SMS                                                         OF GFCTB0C6
BI0412         MOVE GFCTK7-VISUAL-COMPULS TO CINDCD-VSLAO-COMP
BI0412                                                      OF GFCTB0C6
BI0810         MOVE GFCTK7-CPSSOA-TARIFA  TO CPSSOA-SERVC-TARIF
BI0810                                                      OF GFCTB0C6
BI0412     END-IF.
BI0412     IF  GFCTK7-TIPO-ADESAO      EQUAL 'I'
SMS            MOVE ' '                TO CINDCD-EXCEC-ADSAO
SMS                                                         OF GFCTB0C6
BI0412         MOVE GFCTK7-PRMSS-INDVD       TO CIDTFD-PRMSS-INDVD
SMS                                                         OF GFCTB0C6
BI0412         MOVE ZEROS                    TO CINDCD-VSLAO-COMP
BI0412                                                      OF GFCTB0C6
           END-IF.
BI0412     IF  GFCTK7-TIPO-ADESAO      EQUAL 'A'
BI0412         IF GFCTK7-SOLICITA-EXCECAO   EQUAL  1
BI0412             MOVE 'S'            TO CINDCD-EXCEC-ADSAO
BI0412                                                      OF GFCTB0C6
BI0412         ELSE
BI0412             MOVE ' '            TO CINDCD-EXCEC-ADSAO
BI0412                                                      OF GFCTB0C6
BI0412         END-IF
BI0412         MOVE GFCTK7-PRMSS-INDVD       TO CIDTFD-PRMSS-INDVD
BI0412                                                      OF GFCTB0C6
BI0412         MOVE GFCTK7-VISUAL-COMPULS    TO CINDCD-VSLAO-COMP
BI0412                                                      OF GFCTB0C6
BI0412         MOVE GFCTK7-CPSSOA-TARIFA     TO CPSSOA-SERVC-TARIF
BI0412                                                      OF GFCTB0C6
BI0412     END-IF.

           MOVE GFCTK7-TOTAL           TO CINDCD-AGPTO-TOT  OF GFCTB0C6.
           MOVE '0001-01-01-01.01.01.000001'
                                       TO HMANUT-REG        OF GFCTB0C6.

           EXEC SQL
             INSERT INTO DB2PRD.PRMSS_ADSAO_PCOTE
                  (CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CSEQ_AGPTO_CTA,
                   CFUNC_MANUT_INCL,
                   CFUNC_MANUT,
                   DFIM_PRMSS_PCOTE,
                   CINDCD_ADSAO_INDVD,
                   CINDCD_AGPTO_TOT,
                   HMANUT_REG,
                   HINCL_REG_SIST,
                   CINDCD_EXCEC_ADSAO,
MAR10              CIDTFD_PRMSS_INDVD,
BI0412             CINDCD_VSLAO_COMP,
BI0810             CPSSOA_SERVC_TARIF)
               VALUES
                  (:GFCTB0C6.CSERVC-TARIF,
                   :GFCTB0C6.CAGPTO-CTA,
                   :GFCTB0C6.DINIC-PRMSS-PCOTE,
                   :GFCTB0C6.CSEQ-AGPTO-CTA,
                   :GFCTB0C6.CFUNC-MANUT-INCL,
                   :GFCTB0C6.CFUNC-MANUT,
                   :GFCTB0C6.DFIM-PRMSS-PCOTE,
                   :GFCTB0C6.CINDCD-ADSAO-INDVD,
                   :GFCTB0C6.CINDCD-AGPTO-TOT,
                   :GFCTB0C6.HMANUT-REG,
                   CURRENT TIMESTAMP,
                   :GFCTB0C6.CINDCD-EXCEC-ADSAO,
MAR10              :GFCTB0C6.CIDTFD-PRMSS-INDVD,
BI0412             :GFCTB0C6.CINDCD-VSLAO-COMP,
BI0810             :GFCTB0C6.CPSSOA-SERVC-TARIF)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0003'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C6'      TO WRK-NOM-TAB
               MOVE 'PRMSS_ADSAO_PCOTE' TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE '0260'             TO GFCT0M-LOCAL
               MOVE  0003              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2951-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTER MAIOR SEQUENCIA                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       29511-OBTER-MAIOR-SEQ           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTK7-CAGPTO         TO CAGPTO-CTA         OF GFCTB0C6.
           MOVE GFCTK7-CPACOTE        TO CSERVC-TARIF       OF GFCTB0C6.
           MOVE GFCTK7-DATA-INICIO    TO DINIC-PRMSS-PCOTE  OF GFCTB0C6.

           EXEC SQL
               SELECT   MAX(CSEQ_AGPTO_CTA)
               INTO     :GFCTB0C6.CSEQ-AGPTO-CTA
               FROM     DB2PRD.PRMSS_ADSAO_PCOTE
               WHERE    CAGPTO_CTA        = :GFCTB0C6.CAGPTO-CTA
                 AND    CSERVC_TARIF      = :GFCTB0C6.CSERVC-TARIF
                 AND    DINIC_PRMSS_PCOTE = :GFCTB0C6.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C6'      TO WRK-NOM-TAB
               MOVE 'PRMSS_ADSAO_PCOTE' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0270'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -305
               MOVE 1                  TO WRK-SEQUENCIA
           ELSE
               COMPUTE WRK-SEQUENCIA = (CSEQ-AGPTO-CTA OF GFCTB0C6 + 1)
           END-IF.

      *----------------------------------------------------------------*
       29511-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *GERAR PAB                                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2952-GERAR-PAB                  SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTK7-CAGPTO          TO CAGPTO-CTA        OF GFCTB0D0.
           MOVE GFCTK7-CPACOTE         TO CSERVC-TARIF      OF GFCTB0D0.
           MOVE GFCTK7-DATA-INICIO     TO DINIC-PRMSS-PCOTE OF GFCTB0D0.
           MOVE WRK-SEQUENCIA          TO CSEQ-AGPTO-CTA    OF GFCTB0D0.
           MOVE 237                    TO CEMPR-INC         OF GFCTB0D0.
           MOVE GFCTK7-CAGENCIA        TO CDEPDC            OF GFCTB0D0.
           MOVE GFCTK7-CPAB            TO CPOSTO-SERVC      OF GFCTB0D0.

           EXEC SQL
             INSERT INTO DB2PRD.PRMSS_GRP_PAB
                  (CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CSEQ_AGPTO_CTA,
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC)
               VALUES
                  (:GFCTB0D0.CSERVC-TARIF,
                   :GFCTB0D0.CAGPTO-CTA,
                   :GFCTB0D0.DINIC-PRMSS-PCOTE,
                   :GFCTB0D0.CSEQ-AGPTO-CTA,
                   :GFCTB0D0.CEMPR-INC,
                   :GFCTB0D0.CDEPDC,
                   :GFCTB0D0.CPOSTO-SERVC)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0003'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D0'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_PAB'    TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE '0280'             TO GFCT0M-LOCAL
               MOVE  0003              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2952-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *GEARAR AGENCIA                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2953-GERAR-AGENCIA              SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTK7-CAGPTO          TO CAGPTO-CTA        OF GFCTB0C7.
           MOVE GFCTK7-CPACOTE         TO CSERVC-TARIF      OF GFCTB0C7.
           MOVE GFCTK7-DATA-INICIO     TO DINIC-PRMSS-PCOTE OF GFCTB0C7.
           MOVE WRK-SEQUENCIA          TO CSEQ-AGPTO-CTA    OF GFCTB0C7.
           MOVE 237                    TO CEMPR-INC         OF GFCTB0C7.
           MOVE GFCTK7-CAGENCIA        TO CDEPDC            OF GFCTB0C7.

           EXEC SQL
             INSERT INTO DB2PRD.PRMSS_GRP_DEPDC
                  (CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CSEQ_AGPTO_CTA,
                   CEMPR_INC,
                   CDEPDC)
               VALUES
                  (:GFCTB0C7.CSERVC-TARIF,
                   :GFCTB0C7.CAGPTO-CTA,
                   :GFCTB0C7.DINIC-PRMSS-PCOTE,
                   :GFCTB0C7.CSEQ-AGPTO-CTA,
                   :GFCTB0C7.CEMPR-INC,
                   :GFCTB0C7.CDEPDC)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0003'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C7'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_AG'     TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE '0290'             TO GFCT0M-LOCAL
               MOVE  0003              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2953-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *GERAR EMPRESA                                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2954-GERAR-EMPRESA              SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTK7-CAGPTO          TO CAGPTO-CTA        OF GFCTB0C8.
           MOVE GFCTK7-CPACOTE         TO CSERVC-TARIF      OF GFCTB0C8.
           MOVE GFCTK7-DATA-INICIO     TO DINIC-PRMSS-PCOTE OF GFCTB0C8.
           MOVE WRK-SEQUENCIA          TO CSEQ-AGPTO-CTA    OF GFCTB0C8.
           MOVE GFCTK7-CAGENCIA        TO CJUNC-DEPDC-DEB   OF GFCTB0C8.
           MOVE GFCTK7-CCONTA          TO CNRO-CTA-DEB      OF GFCTB0C8.
           MOVE ZEROS                  TO CRZ-CTA-DEB       OF GFCTB0C8
                                          CLCTO-CTA-DEB     OF GFCTB0C8.

           EXEC SQL
             INSERT INTO DB2PRD.PRMSS_GRP_EMPR
                  (CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CSEQ_AGPTO_CTA,
                   CJUNC_DEPDC_DEB,
                   CRZ_CTA_DEB,
                   CNRO_CTA_DEB,
                   CLCTO_CTA_DEB)
               VALUES
                  (:GFCTB0C8.CSERVC-TARIF,
                   :GFCTB0C8.CAGPTO-CTA,
                   :GFCTB0C8.DINIC-PRMSS-PCOTE,
                   :GFCTB0C8.CSEQ-AGPTO-CTA,
                   :GFCTB0C8.CJUNC-DEPDC-DEB,
                   :GFCTB0C8.CRZ-CTA-DEB,
                   :GFCTB0C8.CNRO-CTA-DEB,
                   :GFCTB0C8.CLCTO-CTA-DEB)
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0003'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C8'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_EMPR'   TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE '0300'             TO GFCT0M-LOCAL
               MOVE  0003              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2954-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *GERAR POSTAL                                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2955-GERAR-POSTAL-EXPRE         SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTK7-CAGPTO          TO CAGPTO-CTA        OF GFCTB0D1.
           MOVE GFCTK7-CPACOTE         TO CSERVC-TARIF      OF GFCTB0D1.
           MOVE GFCTK7-DATA-INICIO     TO DINIC-PRMSS-PCOTE OF GFCTB0D1.
           MOVE WRK-SEQUENCIA          TO CSEQ-AGPTO-CTA    OF GFCTB0D1.
           MOVE 237                    TO CEMPR-INC         OF GFCTB0D1.
           MOVE GFCTK7-CAGENCIA        TO CDEPDC            OF GFCTB0D1.
           MOVE GFCTK7-CPAB            TO CPOSTO-SERVC      OF GFCTB0D1.

           EXEC SQL
             INSERT INTO DB2PRD.PRMSS_GRP_PSTAL
                  (CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CSEQ_AGPTO_CTA,
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC)
               VALUES
                  (:GFCTB0D1.CSERVC-TARIF,
                   :GFCTB0D1.CAGPTO-CTA,
                   :GFCTB0D1.DINIC-PRMSS-PCOTE,
                   :GFCTB0D1.CSEQ-AGPTO-CTA,
                   :GFCTB0D1.CEMPR-INC,
                   :GFCTB0D1.CDEPDC,
                   :GFCTB0D1.CPOSTO-SERVC)
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0003'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D1'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_PSTAL'  TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE '0310'             TO GFCT0M-LOCAL
               MOVE  0003              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2955-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *GERAR CLIENTE                                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2956-GERAR-CLIENTE              SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTK7-CAGPTO          TO CAGPTO-CTA        OF GFCTB0I0.
           MOVE GFCTK7-CPACOTE         TO CSERVC-TARIF      OF GFCTB0I0.
           MOVE GFCTK7-DATA-INICIO     TO DINIC-PRMSS-PCOTE OF GFCTB0I0.
           MOVE WRK-SEQUENCIA          TO CSEQ-AGPTO-CTA    OF GFCTB0I0.
ST2506*    MOVE GFCTK7-CGC-CPF         TO CCGC-CPF          OF GFCTB0I0.
ST2506*    MOVE GFCTK7-FILIAL          TO CFLIAL-CGC        OF GFCTB0I0.
ST2506     MOVE GFCTK7-CGC-CPF         TO CCGC-CPF-ST       OF GFCTB0I0.
ST2506     MOVE GFCTK7-FILIAL          TO CFLIAL-CGC-ST     OF GFCTB0I0.
           MOVE GFCTK7-CONTROLE        TO CCTRL-CPF-CGC     OF GFCTB0I0.

           EXEC SQL
             INSERT INTO DB2PRD.TPRMSS_GRP_CLI
                  (CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CSEQ_AGPTO_CTA,
ST2506*            CCGC_CPF,
ST2506*            CCTRL_CPF_CGC,
ST2506*            CFLIAL_CGC)
ST2506             CCGC_CPF_ST,
ST2506             CCTRL_CPF_CGC_ST,
ST2506             CFLIAL_CGC_ST)
               VALUES
                  (:GFCTB0I0.CSERVC-TARIF,
                   :GFCTB0I0.CAGPTO-CTA,
                   :GFCTB0I0.DINIC-PRMSS-PCOTE,
                   :GFCTB0I0.CSEQ-AGPTO-CTA,
ST2506*            :GFCTB0I0.CCGC-CPF,
ST2506*            :GFCTB0I0.CCTRL-CPF-CGC,
ST2506*            :GFCTB0I0.CFLIAL-CGC)
ST2506             :GFCTB0I0.CCGC-CPF-ST,
ST2506             :GFCTB0I0.CCTRL-CPF-CGC-ST,
ST2506             :GFCTB0I0.CFLIAL-CGC-ST)
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0003'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0I0'      TO WRK-NOM-TAB
               MOVE 'TPRMSS_GRP_CLI'   TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE '0320'             TO GFCT0M-LOCAL
               MOVE  0003              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2956-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *GERAR SEGMENTO                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2957-GERAR-SEGMENTO             SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTK7-CPACOTE         TO CSERVC-TARIF      OF GFCTB0D2.
           MOVE GFCTK7-CAGPTO          TO CAGPTO-CTA        OF GFCTB0D2.
           MOVE WRK-SEQUENCIA          TO CSEQ-AGPTO-CTA    OF GFCTB0D2.
           MOVE GFCTK7-DATA-INICIO     TO DINIC-PRMSS-PCOTE OF GFCTB0D2.
           MOVE GFCTK7-CSEGMENTO       TO CSGMTO-GSTAO-TARIF
                                                            OF GFCTB0D2.

           EXEC SQL
             INSERT INTO DB2PRD.PRMSS_GRP_SGMTO
                  (CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CSEQ_AGPTO_CTA,
                   CSGMTO_GSTAO_TARIF)
               VALUES
                  (:GFCTB0D2.CSERVC-TARIF,
                   :GFCTB0D2.CAGPTO-CTA,
                   :GFCTB0D2.DINIC-PRMSS-PCOTE,
                   :GFCTB0D2.CSEQ-AGPTO-CTA,
                   :GFCTB0D2.CSGMTO-GSTAO-TARIF)
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0003'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D2'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_SGMTO'  TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE '0330'             TO GFCT0M-LOCAL
               MOVE  0003              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2957-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *GERAR MUNICIPIO                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2958-GERAR-MUNICIPIO            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTK7-CAGPTO          TO CAGPTO-CTA        OF GFCTB0C9.
           MOVE GFCTK7-CPACOTE         TO CSERVC-TARIF      OF GFCTB0C9.
           MOVE GFCTK7-DATA-INICIO     TO DINIC-PRMSS-PCOTE OF GFCTB0C9.
           MOVE WRK-SEQUENCIA          TO CSEQ-AGPTO-CTA    OF GFCTB0C9.
           MOVE GFCTK7-CMUNIC          TO CMUN-IBGE         OF GFCTB0C9.

           EXEC SQL
             INSERT INTO DB2PRD.PRMSS_GRP_MUN
                  (CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CSEQ_AGPTO_CTA,
                   CMUN_IBGE)
               VALUES
                  (:GFCTB0C9.CSERVC-TARIF,
                   :GFCTB0C9.CAGPTO-CTA,
                   :GFCTB0C9.DINIC-PRMSS-PCOTE,
                   :GFCTB0C9.CSEQ-AGPTO-CTA,
                   :GFCTB0C9.CMUN-IBGE)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0003'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C9'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_MUN'    TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE '0340'             TO GFCT0M-LOCAL
               MOVE  0003              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2958-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *GERAR UF                                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2959-GERAR-UF                   SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTK7-CAGPTO          TO CAGPTO-CTA        OF GFCTB0D3.
           MOVE GFCTK7-CPACOTE         TO CSERVC-TARIF      OF GFCTB0D3.
           MOVE GFCTK7-DATA-INICIO     TO DINIC-PRMSS-PCOTE OF GFCTB0D3.
           MOVE WRK-SEQUENCIA          TO CSEQ-AGPTO-CTA    OF GFCTB0D3.
           MOVE GFCTK7-CUF             TO CSGL-UF           OF GFCTB0D3.

           EXEC SQL
             INSERT INTO DB2PRD.PRMSS_GRP_UF
                  (CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CSEQ_AGPTO_CTA,
                   CSGL_UF)
               VALUES
                  (:GFCTB0D3.CSERVC-TARIF,
                   :GFCTB0D3.CAGPTO-CTA,
                   :GFCTB0D3.DINIC-PRMSS-PCOTE,
                   :GFCTB0D3.CSEQ-AGPTO-CTA,
                   :GFCTB0D3.CSGL-UF)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0003'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D3'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_UF'     TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE '0350'             TO GFCT0M-LOCAL
               MOVE  0003              TO GFCTK8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTK8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2959-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

BI0810******************************************************************
BI0810*VERIFICA DUPLICADO                                              *
BI0810******************************************************************
BI0810*----------------------------------------------------------------*
BI0810 2960-VERIFICA-DUPLICADO         SECTION.
BI0810*----------------------------------------------------------------*
BI0810
BI0810     IF GFCTK7-TIPO-ADESAO  EQUAL  'I'
BI0412     OR GFCTK7-TIPO-ADESAO  EQUAL  'A'
BI0810        MOVE 'S' TO WRK-ACHOU-DUPLICIDADE
BI0810     ELSE
BI0810        IF CINDCD-ADSAO-INDVD OF GFCTB0C6  EQUAL 'I'
BI0412        OR CINDCD-ADSAO-INDVD OF GFCTB0C6  EQUAL 'A'
BI0810           MOVE 'S' TO WRK-ACHOU-DUPLICIDADE
BI0810        ELSE
BI0810           IF (CPSSOA-SERVC-TARIF OF GFCTB0C6 EQUAL '3') OR
BI0810              (GFCTK7-CPSSOA-TARIFA EQUAL '3')
BI0810              MOVE 'S' TO WRK-ACHOU-DUPLICIDADE
BI0810           ELSE
BI0810              IF GFCTK7-CPSSOA-TARIFA EQUAL CPSSOA-SERVC-TARIF
BI0810                                                   OF GFCTB0C6
BI0810                 MOVE 'S' TO WRK-ACHOU-DUPLICIDADE
BI0810              END-IF
BI0810           END-IF
BI0810        END-IF
BI0810     END-IF.
BI0810
BI0810*----------------------------------------------------------------*
BI0810 2960-99-FIM.                    EXIT.
BI0810*----------------------------------------------------------------*
BI0810
BI0810******************************************************************
BI0810*VERIFICA FUTURO                                                 *
BI0810******************************************************************
BI0810*----------------------------------------------------------------*
BI0810 2970-VERIFICA-FUTURO            SECTION.
BI0810*----------------------------------------------------------------*
BI0810
BI0810     IF GFCTK7-TIPO-ADESAO  EQUAL  'I'
BI0412     OR GFCTK7-TIPO-ADESAO  EQUAL  'A'
BI0810        MOVE 'S' TO WRK-ACHOU-FUTURO
BI0810     ELSE
BI0810        IF CINDCD-ADSAO-INDVD OF GFCTB0C6  EQUAL 'I'
BI0412        OR CINDCD-ADSAO-INDVD OF GFCTB0C6  EQUAL 'A'
BI0810           MOVE 'S' TO WRK-ACHOU-FUTURO
BI0810        ELSE
BI0810           IF (CPSSOA-SERVC-TARIF OF GFCTB0C6 EQUAL '3') OR
BI0810              (GFCTK7-CPSSOA-TARIFA EQUAL '3')
BI0810              MOVE 'S' TO WRK-ACHOU-FUTURO
BI0810           ELSE
BI0810              IF GFCTK7-CPSSOA-TARIFA EQUAL CPSSOA-SERVC-TARIF
BI0810                                                   OF GFCTB0C6
BI0810                 MOVE 'S' TO WRK-ACHOU-FUTURO
BI0810              END-IF
BI0810           END-IF
BI0810        END-IF
BI0810     END-IF.
BI0810
BI0810*----------------------------------------------------------------*
BI0810 2970-99-FIM.                    EXIT.
BI0810*----------------------------------------------------------------*

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
