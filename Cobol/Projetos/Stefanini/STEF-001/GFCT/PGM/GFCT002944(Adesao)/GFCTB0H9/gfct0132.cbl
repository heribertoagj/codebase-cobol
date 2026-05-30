      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0132.
       AUTHOR.     CARLA FLACH ZANETTI.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0132                                    *
      *    PROGRAMADOR.:   CARLA FLACH ZANETTI     - CPM PATO BRANCO   *
      *    ANALISTA CPM:   FELIPE ALEX SOMMER      - CPM PATO BRANCO   *
      *    ANALISTA....:   MARCELO CREMM           - PROCWORK /GP.50   *
      *    DATA........:   22/03/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   INCLUSAO DE ADESAO COMPULSORIA              *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                        INCLUDE/BOOK     *
      *                   DB2PRD.ADSAO_COMP_PCOTE       GFCTB001       *
      *                   DB2PRD.PARM_DATA_PROCM        GFCTB0A1       *
      *                   DB2PRD.ADSAO_GRP_DEPDC        GFCTB002       *
      *                   DB2PRD.ADSAO_GRP_EMPR         GFCTB003       *
      *                   DB2PRD.ADSAO_GRP_MUN          GFCTB004       *
      *                   DB2PRD.ADSAO_GRP_PAB          GFCTB005       *
      *                   DB2PRD.ADSAO_GRP_PSTAL        GFCTB006       *
      *                   DB2PRD.ADSAO_GRP_SGMTO        GFCTB007       *
      *                   DB2PRD.ADSAO_GRP_UF           GFCTB008       *
      *                   DB2PRD.TADSAO_GRP_CLI         GFCTB0H9       *
      *                   DB2PRD.V01UF                  DCITV002       *
      *                   DB2PRD.PRMSS_ADSAO_PCOTE      GFCTB0C6       *
      *                   DB2PRD.PRMSS_GRP_DEPDC        GFCTB0C7       *
      *                   DB2PRD.PRMSS_GRP_EMPR         GFCTB0C8       *
      *                   DB2PRD.PRMSS_GRP_MUN          GFCTB0C9       *
      *                   DB2PRD.PRMSS_GRP_PAB          GFCTB0D0       *
      *                   DB2PRD.PRMSS_GRP_PSTAL        GFCTB0D1       *
      *                   DB2PRD.PRMSS_GRP_SGMTO        GFCTB0D2       *
      *                   DB2PRD.PRMSS_GRP_UF           GFCTB0D3       *
      *                   DB2PRD.TPRMSS_GRP_CLI         GFCTB0I0       *
      *                   DB2PRD.TCAD_CORSP_BCRIO       YMKCB001       *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTLH - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCTLI - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCTBR - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5060 *
      *    I#GFCTBS - AREA DE COMUNICACAO - SAIDA   DO MODULO GFCT5060 *
      *    I#GFCTIU - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5110 *
      *    I#GFCTIV - AREA DE COMUNICACAO - SAIDA   DO MODULO GFCT5110 *
      *    I#GFCTFZ - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5522 *
      *    I#GFCTG1 - AREA DE COMUNICACAO - SAIDA   DO MODULO GFCT5522 *
      *    I#GFCTG2 - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5523 *
      *    I#GFCTG3 - AREA DE COMUNICACAO - SAIDA   DO MODULO GFCT5523 *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS                      *
      *    I#MESUM6 - AREA DE COMUNICACAO COM MODULO MESU9015          *
      *    I#MESUM8 - AREA DE COMUNICACAO COM MODULO MESU9018          *
      *    I#RURC88 - AREA DE COMUNICACAO COM MODULO RURC9020          *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS                     *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                   *
      *    MESU9015 - CONSISTIR AGENCIA                                *
      *    MESU9018 - CONSISTIR PAB                                    *
      *    GFCT5060 - CONSISTIR AGENCIA E CONTA                        *
      *    GFCT5110 - PESQUISAR CLIE                                   *
      *    GFCT5522 - VERIFICAR DISPONIBILIDADE DO ON-LINE             *
      *    POOL0110 - CONSISTIR CLIENTE (CPF)                          *
      *    DCIT8000 - VERIFICA UF                                      *
      *    RURC9020 - VERIFICA MUNICIPIO                               *
      *    POOL1470 - CONSISTENCIA DE DATAS                            *
      *    POOL0025 - SOMAR/SUBTRAIR NUMERO DE DIAS DE UMA DATA        *
      *================================================================*
      * ULTIMA ALTERACAO: MARCELO CREMM - PROCWORK - SETEMBRO/2006.    *
      * ===========================================================    *
      *                                                                *
      * OBJETIVO: PASSAR A NAO DEIXAR INCLUIR ADESAO COMPULSORIA       *
      *           QUANDO JA HOUVER O MESMO AGRUPAMENTO COM QUALQUER    *
      *           OUTRO PACOTE SEJA ELE VIGENTE OU FUTURO.             *
      *                                                                *
      *  **** ALTERACOES FEITAS COM COMENTARIO PWIE / PWIS             *
      *                                        ====   ====             *
      *================================================================*
      *                    A L T E R A C A O                           *
      *----------------------------------------------------------------*
      *                                                                *
      *    ANALISTA     : MARCUS VINICIUS          - PROCWORK          *
      *    DATA         : 20/08/2008                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      INCLUS O DO TRATAMENTO PARA BRADESCO EXPRESSO, ACESSANDO  *
      *      A BASE YMKCB001 DE CORRESPONDENTE BANCARIO, SEPARANDO-O   *
      *      DO TRATAMENTO DE BANCO POSTAL.                            *
      *                                                                *
BI0810*================================================================*
BI0810*                    A L T E R A C A O                           *
BI0810*----------------------------------------------------------------*
BI0810*                                                                *
BI0810*    ANALISTA     : UBIRAJARA MIRANDA(BIRA)  - PROCWORK          *
BI0810*    DATA         : AGO/2010                                     *
BI0810*                                                                *
BI0810*    OBJETIVO     :                                              *
BI0810*    - INCLUSAO DO CAMPO (TIPO DE PESSOA) NA TABELA GFCTB001,    *
BI0810*      CONSISTE ADESAO    PARA FISICA, JURIDICA OU AMBAS.        *
BI0810*                                                                *
      *================================================================*
      *                                                                *
      *    ANALISTA....:  CIBELE          - SONDA IT                   *
      *    DATA........:  05 / 2014                                    *
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

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77 FILLER                       PIC  X(050) VALUE
             '* INICIO DA AREA DE WORKING *'.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       77 FILLER                       PIC  X(050) VALUE
             '* AREA PARA VARIAVEIS AUXILIARES *'.
      *---------------------------------------------------------------*

       77 WRK-DINIC                    PIC  9(008) VALUE ZEROS.
       77 WRK-DFIM                     PIC  9(008) VALUE ZEROS.
       77 WRK-DINIC-PCOTE              PIC  9(008) VALUE ZEROS.
       77 WRK-DFIM-PCOTE               PIC  9(008) VALUE ZEROS.
       77 WRK-DPROCM                   PIC  9(008) VALUE ZEROS.
       77 WRK-MODULO                   PIC  X(008) VALUE SPACES.
       77 WRK-NOME-TAB                 PIC  X(011) VALUE SPACES.
       77 WRK-FIM                      PIC  X(001) VALUE SPACES.
       77 WRK-FIM-CSR01-GFCTB001       PIC  X(001) VALUE SPACES.
       77 WRK-ACHOU-DUPLICIDADE        PIC  X(001) VALUE SPACES.
       77 WRK-ACHOU-FUTURO             PIC  X(001) VALUE SPACES.
       77 WRK-DATA-1470                PIC  9(008) VALUE ZEROS.
       77 WRK-SEQUENCIA                PIC  9(009) COMP-3
                                                   VALUE ZEROS.

       01 WRK-FUNC-BDSCO.
          05 WRK-FUNC-BDSCO-N          PIC 9(007)  VALUE ZEROS.

       01 WRK-DATA.
          05 WRK-DIA                   PIC  9(002) VALUE ZEROS.
          05 FILLER                    PIC  X(001) VALUE '.'.
          05 WRK-MES                   PIC  9(002) VALUE ZEROS.
          05 FILLER                    PIC  X(001) VALUE '.'.
          05 WRK-ANO                   PIC  9(004) VALUE ZEROS.

       01 WRK-DATA-AUX.
          05 WRK-DIA-AUX               PIC  9(002) VALUE ZEROS.
          05 WRK-MES-AUX               PIC  9(002) VALUE ZEROS.
          05 WRK-ANO-AUX               PIC  9(004) VALUE ZEROS.

       01 WRK-DATA-INV.
          05 WRK-ANO-INV               PIC  9(004) VALUE ZEROS.
          05 WRK-MES-INV               PIC  9(002) VALUE ZEROS.
          05 WRK-DIA-INV               PIC  9(002) VALUE ZEROS.

       01 WRK-MSG01.
          05 FILLER                    PIC  X(026) VALUE
                'ERRO NA CHAMADA AO MODULO '.
          05 WRK-NOME-MOD              PIC  X(008) VALUE SPACES.

       01 WRK-CPAB-9-05                PIC  9(005) VALUE ZEROS.
       01 WRK-FILLER REDEFINES WRK-CPAB-9-05.
          05 FILLER                    PIC  9(02).
          05 WRK-CPAB-9-03             PIC  9(03).

ST2506*----------------------------------------------------------------*
ST2506*01 FILLER                       PIC  X(050) VALUE
ST2506*      '* AREAS DA POOL0110 *'.
ST2506*----------------------------------------------------------------*
ST2506*
ST2506*01 WRK-CGC-CPF-0110.
ST2506*   05 WRK-CPF-0110              PIC  9(009) COMP-3
ST2506*                                            VALUE ZEROS.
ST2506*   05 WRK-FILIAL-0110           PIC  9(005) COMP-3
ST2506*                                            VALUE ZEROS.
ST2506*
ST2506*01 WRK-CONTROLE-0110            PIC  X(002) VALUE SPACES.

      *----------------------------------------------------------------*
       01 FILLER                       PIC  X(050) VALUE
             '* AREA PARA POOL1640 *'.
      *----------------------------------------------------------------*

       01 WRK-DATA-01.
          05 FILLER                    PIC  X(002) VALUE SPACES.
          05 WRK-DATA-01-ENT           PIC  9(008) VALUE ZEROS.
       01 WRK-DATA-01-R REDEFINES WRK-DATA-01
                                       PIC  X(010).

       01 WRK-1640-CAMPOS.
          05 WRK-1640-DATA-ENTRADA     PIC  X(010) VALUE SPACES.
          05 WRK-1640-FORMATO-ENTRADA  PIC 9(002)  VALUE ZEROS.
          05 WRK-1640-TIPO-INCREMENTO  PIC X(002)  VALUE SPACES.
          05 WRK-1640-INCREMENTO       PIC  9(003) VALUE ZEROS.
          05 WRK-1640-FORMATO-SAIDA    PIC  9(002) VALUE ZEROS.
          05 WRK-1640-DATA-SAIDA       PIC  X(010) VALUE SPACES.
          05 WRK-1640-MENSAGEM         PIC  X(080) VALUE SPACES.

      *----------------------------------------------------------------*
       01 FILLER                       PIC  X(050) VALUE
             '* AREA PARA POOL0025 *'.
      *----------------------------------------------------------------*

       01 WRK-DATA-ABERT               PIC  9(009) COMP-3
                                                   VALUE ZEROS.
       01 WRK-PRAZO                    PIC S9(005) COMP-3
                                                   VALUE ZEROS.
       01 WRK-DATA-VENC                PIC  9(009) COMP-3
                                                   VALUE ZEROS.

       01 WRK-DATA-0025                PIC  9(008) VALUE ZEROS.


ST2506 01  WRK-9-02-X.
ST2506     05 WRK-9-02                 PIC  9(02) VALUE ZEROS.
ST2506
ST2506*----------------------------------------------------------------*
ST2506 01 WRK-BRAD2000                 PIC  X(08)  VALUE
ST2506       'BRAD2000'.
ST2506 
ST2506 01 WRK-AREA-BRAD2000.
ST2506    05 WRK-CAD-CPFCGC.
ST2506       10 WRK-CAD-NUMERO         PIC X(09)           VALUE SPACES.
ST2506       10 WRK-CAD-FILIAL         PIC X(04)           VALUE SPACES.        
ST2506 77  WRK-CAD-CONTROLE            PIC 9(02)           VALUE ZEROS.
ST2506
ST2506*------------------------------------*----------------------------*
ST2506 01 FILLER                       PIC  X(051) VALUE
ST2506       '* AREA PARA MODULO GFCT20PJ *'.
ST2506*---------------------------------------------------------------*
ST2506 01 WRK-GFCT20PJ                 PIC X(08)   VALUE
ST2506       'GFCT20PJ'.
ST2506
ST2506 01 WRK-AREA-GFCT20PJ.
ST2506 COPY 'GFCTW2PJ'.
ST2506
      *----------------------------------------------------------------*
       01 FILLER                       PIC  X(051) VALUE
             '* AREA PARA MODULO DCIT8000 *'.
      *---------------------------------------------------------------*

       01 WRK-DCIT-UNIDADE-FEDERATIVA.
          05 WRK-AREA-DCLGEN-DCITV002  PIC X(070)  VALUE SPACES.
          05 WRK-DCIT-CODIGO-RETORNO   PIC 9(002)  VALUE ZEROS.

       01 WRK-POOL7100-DCIT            PIC X(107)  VALUE SPACES.
       01 WRK-SQLCA-DCIT               PIC X(136)  VALUE SPACES.

      *----------------------------------------------------------------*
       01 FILLER                       PIC  X(051) VALUE
             '* AREA PARA INCLUDES *'.
      *---------------------------------------------------------------*

       COPY KCIMW184.

       COPY 'I#GFCTIU'.

       COPY 'I#GFCTIV'.

       COPY 'I#GFCTBR'.

       COPY 'I#GFCTBS'.

       COPY 'I#GFCTFZ'.

       COPY 'I#GFCTG1'.

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

       COPY 'I#MESUM6'.

       COPY 'I#MESUM8'.

       COPY 'I#RURC88'.

      *----------------------------------------------------------------*
       01 FILLER                       PIC  X(050) VALUE
             '* AREA PARA TABELA DB2 *'.
      *---------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
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
             INCLUDE GFCTB0I0
           END-EXEC.

           EXEC SQL
             INCLUDE DCITV002
           END-EXEC.

      *----------------------------------------------------------------*
       01 FILLER                       PIC  X(050) VALUE
             '* AREA PARA DECLARACAO DE CURSORES *'.
      *---------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR01-GFCTB001 CURSOR FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
BI0810              CPSSOA_SERVC_TARIF
             FROM DB2PRD.ADSAO_COMP_PCOTE
PWIE          WHERE CAGPTO_CTA = :GFCTB001.CAGPTO-CTA
              AND DFIM_ADSAO_COMP > :GFCTB001.DFIM-ADSAO-COMP
           END-EXEC.

           EXEC SQL
             DECLARE CSR02-GFCTB001 CURSOR FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_ADSAO_COMP,
                   CSEQ_AGPTO_CTA,
BI0810              CPSSOA_SERVC_TARIF
             FROM DB2PRD.ADSAO_COMP_PCOTE
             WHERE
                   CAGPTO_CTA = :GFCTB001.CAGPTO-CTA AND
                   DINIC_ADSAO_COMP > :GFCTB001.DINIC-ADSAO-COMP
           END-EXEC.

      *----------------------------------------------------------------*
       01 FILLER                       PIC  X(050) VALUE
             '* FIM DA AREA DE WORKING *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTLH'.

       COPY 'I#GFCTLI'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTLH-ENTRADA
                                                      GFCTLI-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *    CHAMA AS ROTINAS A SEREM EXECUTADAS                         *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-00-INICIAR SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    MONTA AREA DE SAIDA, VERIFICA DISPONIBILIDADE DO SISTEMA E  *
      *    CONSISTE DADOS                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES TO GFCT0M-AREA-ERROS
                          GFCTLI-SAIDA.

           INITIALIZE GFCTLI-SAIDA
                      GFCT0M-AREA-ERROS.

           PERFORM 1100-VERIFICAR-ON-LINE.

           PERFORM 1200-MONTAR-AREA-SAIDA.

           PERFORM 1300-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA DISPONIBILIDADE DO SISTEMA                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-VERIFICAR-ON-LINE SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES TO GFCTFZ-ENTRADA
                          GFCTG1-SAIDA.

           INITIALIZE GFCTFZ-ENTRADA
                      GFCTG1-SAIDA.

           MOVE 'GFCT' TO GFCTFZ-SISTEMA.
           MOVE GFCTLH-TRANSACAO TO GFCTFZ-TRANSACAO.
           MOVE GFCTLH-FUNCAO TO GFCTFZ-FUNCAO.
           MOVE GFCTLH-FUNC-BDSCO TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N' TO GFCTFZ-FIM.

           MOVE 'GFCT5522' TO WRK-MODULO.

           CALL 'POOL0081' USING WRK-MODULO
                                 GFCTFZ-ENTRADA
                                 GFCTG1-SAIDA
                                 GFCT0M-AREA-ERROS.

           IF RETURN-CODE NOT EQUAL ZEROS
              MOVE 9 TO GFCTLI-ERRO
              MOVE WRK-MODULO TO WRK-NOME-MOD
                                 GFCT0M-PROGRAMA
              MOVE WRK-MSG01 TO GFCT0M-TEXTO
              MOVE 'APL' TO GFCT0M-TIPO-ACESSO
              MOVE 'GFCT0132' TO GFCT0M-TRANSACAO
              PERFORM 3000-FINALIZAR
           END-IF.

           IF GFCTG1-ERRO EQUAL 9
              IF GFCT0M-TIPO-ACESSO NOT EQUAL 'DB2'
                 MOVE GFCTG1-DESC-MSG-ERRO TO GFCT0M-TEXTO
                 MOVE 'APL' TO GFCT0M-TIPO-ACESSO
              ELSE
                 MOVE GFCTG1-COD-SQL-ERRO TO GFCTLI-COD-SQL-ERRO
              END-IF
              MOVE WRK-MODULO TO GFCT0M-PROGRAMA
              MOVE 'GFCT0132' TO GFCT0M-TRANSACAO
              MOVE GFCTG1-ERRO TO GFCTLI-ERRO
              MOVE GFCTG1-COD-MSG-ERRO TO GFCTLI-COD-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

           IF GFCTG1-CSIT-ONLINE EQUAL 1
              MOVE 9 TO GFCTLI-ERRO
              MOVE 'SISTEMA INDISPONIVEL' TO GFCT0M-TEXTO
              MOVE WRK-MODULO TO GFCT0M-PROGRAMA
              MOVE 'APL' TO GFCT0M-TIPO-ACESSO
              MOVE 'GFCT0132' TO GFCT0M-TRANSACAO
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FORMATA CAMPOS DA AREA DE SAIDA                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-MONTAR-AREA-SAIDA SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES TO GFCTLI-SAIDA.
           INITIALIZE GFCTLI-SAIDA.

           MOVE 1010 TO GFCTLI-LL.
           MOVE ZEROS TO GFCTLI-ZZ.
           MOVE GFCTLH-TRANSACAO TO GFCTLI-TRANSACAO.
           MOVE GFCTLH-FUNCAO TO GFCTLI-FUNCAO.
           MOVE GFCTLH-FUNC-BDSCO TO GFCTLI-FUNC-BDSCO.
           MOVE GFCTLH-TERMINAL TO GFCTLI-TERMINAL.
           MOVE 'S' TO GFCTLI-FIM.
           MOVE GFCTLH-FIXO TO GFCTLI-FIXO.

           MOVE '0000' TO GFCTG2-COD-MSG.

           PERFORM 1210-OBTER-DESC-MSG.

           MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM DESCRICAO DA MENSAGEM                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1210-OBTER-DESC-MSG SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE +100 TO GFCTG2-LL.
           MOVE ZEROS TO GFCTG2-ZZ.
           MOVE GFCTLH-TRANSACAO TO GFCTG2-TRANSACAO.
           MOVE GFCTLH-FUNCAO TO GFCTG2-FUNCAO.
           MOVE GFCTLH-FUNC-BDSCO TO GFCTG2-FUNC-BDSCO.
           MOVE 'N' TO GFCTG2-FIM.
           MOVE 'O' TO GFCTG2-TIPO-PROC.

           MOVE 'GFCT5523' TO WRK-MODULO.

           CALL 'POOL0081' USING WRK-MODULO
                                 GFCTG2-ENTRADA
                                 GFCTG3-SAIDA
                                 GFCT0M-AREA-ERROS.

           IF RETURN-CODE NOT EQUAL ZEROS OR
              GFCTG3-ERRO EQUAL 9
              MOVE 9 TO GFCTLI-ERRO
              MOVE WRK-MODULO TO GFCT0M-PROGRAMA
                                 WRK-NOME-MOD
              MOVE WRK-MSG01 TO GFCT0M-TEXTO
              MOVE 'APL' TO GFCT0M-TIPO-ACESSO
              MOVE 'GFCT0132' TO GFCT0M-TRANSACAO
              PERFORM 3000-FINALIZAR
           END-IF.

           IF GFCTG3-ERRO EQUAL 1
              MOVE 1 TO GFCTLI-ERRO
              MOVE GFCTG3-COD-SQL-ERRO TO GFCTLI-COD-SQL-ERRO
              MOVE GFCTG3-COD-MSG-ERRO TO GFCTLI-COD-MSG-ERRO
              MOVE GFCTG3-DESC-MSG-ERRO TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1210-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA CONSISTIR DADOS                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS SECTION.
      *----------------------------------------------------------------*

           IF GFCTLH-TRANSACAO EQUAL LOW-VALUES OR
              GFCTLH-TRANSACAO EQUAL SPACES OR
              GFCTLH-FUNCAO EQUAL LOW-VALUES OR
              GFCTLH-FUNCAO EQUAL SPACES OR
              GFCTLH-FUNC-BDSCO EQUAL LOW-VALUES OR
              GFCTLH-FUNC-BDSCO EQUAL SPACES OR
              GFCTLH-DATA-INICIO EQUAL LOW-VALUES OR
              GFCTLH-DATA-INICIO EQUAL SPACES OR
              GFCTLH-DATA-FIM EQUAL LOW-VALUES OR
              GFCTLH-DATA-FIM EQUAL SPACES OR
              GFCTLH-DATA-INICIO-PCTE EQUAL LOW-VALUES OR
              GFCTLH-DATA-INICIO-PCTE EQUAL SPACES OR
              GFCTLH-DATA-FIM-PCTE EQUAL LOW-VALUES OR
              GFCTLH-DATA-FIM-PCTE EQUAL SPACES OR
              GFCTLH-CPACOTE EQUAL ZEROS OR
BI0810        (GFCTLH-CPSSOA-TARIFA NOT EQUAL '1' AND '2' AND '3') OR
              (GFCTLH-CAGPTO NOT EQUAL 4 AND 9 AND
              14 AND 15 AND
              16 AND 17 AND
              18 AND 19 AND
              20)
              MOVE 1 TO GFCTLI-ERRO
                        GFCTLI-COD-MSG-ERRO
              MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
              MOVE '0001' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 1310-VERIFICAR-CAGPTO.

           PERFORM 1320-ACESSAR-GFCTB0A1.

      *----------------------------------------------------------------*
       1300-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CONSISTENCIA DO CAMPO GFCTLH-CAGPTO                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1310-VERIFICAR-CAGPTO SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTLH-CAGPTO
           WHEN 4
ST2506          MOVE GFCTLH-CGC-CPF         TO GFCTW2PJ-E-CGC-CNPJ 
ST2506          PERFORM 1311-VALIDAR-FORMATO-PJ

ST2506*         IF GFCTLH-CGC-CPF NOT NUMERIC OR
ST2506*            GFCTLH-CGC-CPF EQUAL ZEROS OR 
ST2506          IF GFCTLH-CGC-CPF EQUAL SPACES OR
ST2506             GFCTLH-CGC-CPF EQUAL LOW-VALUES OR 
ST2506             GFCTW2PJ-S-INVALIDO 
                   MOVE 1 TO GFCTLI-ERRO
                   MOVE 773 TO GFCTLI-COD-MSG-ERRO
                   MOVE '0773' TO GFCTG2-COD-MSG
                   MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
                   PERFORM 1210-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
                END-IF
           WHEN 9
                IF GFCTLH-CSEGMENTO NOT NUMERIC OR
                   GFCTLH-CSEGMENTO EQUAL ZEROS
                   MOVE 1 TO GFCTLI-ERRO
                   MOVE 775 TO GFCTLI-COD-MSG-ERRO
                   MOVE '0775' TO GFCTG2-COD-MSG
                   MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
                   PERFORM 1210-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
                END-IF
           WHEN 14
                IF (GFCTLH-CAGENCIA NOT NUMERIC) OR
                   (GFCTLH-CAGENCIA EQUAL ZEROS AND
                   GFCTLI-TOTAL NOT EQUAL 'S')
                   MOVE 1 TO GFCTLI-ERRO
                   MOVE 774 TO GFCTLI-COD-MSG-ERRO
                   MOVE '0774' TO GFCTG2-COD-MSG
                   MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
                   PERFORM 1210-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
                END-IF
           WHEN 15
                IF GFCTLH-CPAB NOT NUMERIC OR
                   GFCTLH-CAGENCIA NOT NUMERIC OR
                   GFCTLH-CAGENCIA EQUAL ZEROS OR
                   (GFCTLH-CPAB EQUAL ZEROS AND
                   GFCTLI-TOTAL NOT EQUAL 'S')
                   MOVE 1 TO GFCTLI-ERRO
                   MOVE 776 TO GFCTLI-COD-MSG-ERRO
                   MOVE '0776' TO GFCTG2-COD-MSG
                   MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
                   PERFORM 1210-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
                END-IF
           WHEN 16
                IF GFCTLH-CAGENCIA NOT NUMERIC OR
                   GFCTLH-CAGENCIA EQUAL ZEROS OR
                   GFCTLH-CCONTA NOT NUMERIC OR
                   GFCTLH-CCONTA EQUAL ZEROS
                   MOVE 1 TO GFCTLI-ERRO
                   MOVE 777 TO GFCTLI-COD-MSG-ERRO
                   MOVE '0777' TO GFCTG2-COD-MSG
                   MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
                   PERFORM 1210-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
                END-IF
           WHEN 17
                IF GFCTLH-CPAB NOT NUMERIC OR
                   GFCTLH-CAGENCIA NOT NUMERIC OR
                   (GFCTLH-CAGENCIA EQUAL ZEROS AND
                   GFCTLH-CPAB EQUAL ZEROS AND
                   GFCTLI-TOTAL NOT EQUAL 'S')
                   MOVE 1 TO GFCTLI-ERRO
                   MOVE 778 TO GFCTLI-COD-MSG-ERRO
                   MOVE '0778' TO GFCTG2-COD-MSG
                   MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
                   PERFORM 1210-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
                END-IF
           WHEN 18
                IF GFCTLH-CMUNIC EQUAL SPACES OR
                   GFCTLH-CMUNIC EQUAL LOW-VALUES
                   MOVE 1 TO GFCTLI-ERRO
                   MOVE 779 TO GFCTLI-COD-MSG-ERRO
                   MOVE '0779' TO GFCTG2-COD-MSG
                   MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
                   PERFORM 1210-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
                END-IF
           WHEN 19
                IF GFCTLH-CUF EQUAL SPACES OR
                   GFCTLH-CUF EQUAL LOW-VALUES
                   MOVE 1 TO GFCTLI-ERRO
                   MOVE 780 TO GFCTLI-COD-MSG-ERRO
                   MOVE '0780' TO GFCTG2-COD-MSG
                   MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
                   PERFORM 1210-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
                END-IF
           WHEN 20
                IF GFCTLH-CPAB NOT NUMERIC OR
                   GFCTLH-CAGENCIA NOT NUMERIC OR
                   (GFCTLH-CAGENCIA EQUAL ZEROS AND
                   GFCTLH-CPAB EQUAL ZEROS AND
                   GFCTLI-TOTAL NOT EQUAL 'S')
                   MOVE 1 TO GFCTLI-ERRO
                   MOVE 1479 TO GFCTLI-COD-MSG-ERRO
                   MOVE '1479' TO GFCTG2-COD-MSG
                   MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
                   PERFORM 1210-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
                END-IF
           END-EVALUATE.

      *----------------------------------------------------------------*
       1310-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

ST2506*----------------------------------------------------------------*
ST2506 1311-VALIDAR-FORMATO-PJ         SECTION.
ST2506*----------------------------------------------------------------*
ST2506  
ST2506     CALL WRK-GFCT20PJ           USING WRK-AREA-GFCT20PJ.
ST2506
ST2506     IF RETURN-CODE NOT EQUAL ZEROS 
ST2506     OR GFCTW2PJ-S-COD-RETORNO NOT EQUAL '00'
ST2506        MOVE 1 TO GFCTLI-ERRO
ST2506        MOVE 'GFCT0132'   TO GFCT0M-PROGRAMA
ST2506        MOVE 'GFCT20PJ'   TO WRK-NOME-TAB
ST2506        MOVE 'CALL'       TO GFCT0M-TIPO-ACESSO
ST2506        MOVE '1311'       TO GFCTG2-COD-MSG
ST2506        PERFORM 1210-OBTER-DESC-MSG
ST2506        MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
ST2506        PERFORM 3000-FINALIZAR
ST2506     END-IF. 
ST2506                                 
ST2506*----------------------------------------------------------------*
ST2506 1311-99-FIM.                    EXIT.
ST2506*----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA ACESSAR GFCTB0A1 PARA BUSCAR DATA               *
      ******************************************************************
      *----------------------------------------------------------------*
       1320-ACESSAR-GFCTB0A1 SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT' TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ATUAL
             INTO :GFCTB0A1.DPROCM-ATUAL
             FROM DB2PRD.PARM_DATA_PROCM
             WHERE CSIST_PRINC = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0012' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 0012 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB0A1' TO WRK-NOME-TAB
              MOVE 'PARM_DATA_PROCM' TO GFCT0M-NOME-TAB
              MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
              MOVE '0010' TO GFCT0M-LOCAL
              PERFORM 1321-MOVER-ERRO-SQL
              PERFORM 3000-FINALIZAR
           END-IF.

           MOVE DPROCM-ATUAL OF GFCTB0A1
              TO WRK-DATA.
           MOVE WRK-DIA TO WRK-DIA-INV.
           MOVE WRK-MES TO WRK-MES-INV.
           MOVE WRK-ANO TO WRK-ANO-INV.
           MOVE WRK-DATA-INV TO WRK-DPROCM.

      *----------------------------------------------------------------*
       1320-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    MOVE ERRO SQL                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       1321-MOVER-ERRO-SQL SECTION.
      *----------------------------------------------------------------*

           MOVE 9 TO GFCTLI-ERRO.
           MOVE 'GFCT0132' TO GFCT0M-PROGRAMA.
           MOVE 'DB2' TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCA TO GFCT0M-SQLCA-AREA.
           MOVE SQLCODE TO GFCT0M-SQLCODE
                           GFCTLI-COD-SQL-ERRO.
           MOVE SPACES TO GFCTLI-DESC-MSG-ERRO.

           STRING GFCTG3-DESC-MSG WRK-NOME-TAB

              DELIMITED BY '  ' INTO GFCTLI-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1321-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-TRATAR-CAGPTO.

           PERFORM 2200-CONSISTIR-DATAS.

           PERFORM 2300-VERIF-DUPLICIDADE.

           PERFORM 2400-INSERIR.

      *----------------------------------------------------------------*
       2000-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATAMENTO DO CAMPO GFCTLH-CAGPTO                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-TRATAR-CAGPTO SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTLH-CAGPTO
           WHEN 15
                PERFORM 2110-CONSISTIR-PAB

           WHEN 14
                IF GFCTLH-TOTAL NOT EQUAL 'S'
                   PERFORM 2120-CONSISTIR-AGENCIA
                END-IF
           WHEN 16
                PERFORM 2130-CONSISTIR-EMPRESA

           WHEN 17
                PERFORM 2140-CONSISTIR-POSTAL-EXP

PW1334     WHEN 20
PW1334          PERFORM 2140-CONSISTIR-POSTAL-EXP

           WHEN 4
                PERFORM 2150-CONSISTIR-CLIENTE

           WHEN 9
                PERFORM 2160-CONSISTIR-SEGMENTO

           WHEN 18
                PERFORM 2170-CONSISTIR-MUNICIPIO

           WHEN 19
                PERFORM 2180-CONSISTIR-UF
           END-EVALUATE.

      *----------------------------------------------------------------*
       2100-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CONSISTE PAB                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-CONSISTIR-PAB SECTION.
      *----------------------------------------------------------------*

           PERFORM 2120-CONSISTIR-AGENCIA.

           IF GFCTLH-TOTAL NOT EQUAL 'S'
              PERFORM 2111-ACESSAR-MESU9018
              IF COMU-COD-TIPO-POSTO EQUAL 9
                 MOVE 1 TO GFCTLI-ERRO
                 MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
                 MOVE 0594 TO GFCTLI-COD-MSG-ERRO
                 MOVE '0594' TO GFCTG2-COD-MSG
                 PERFORM 1210-OBTER-DESC-MSG
                 MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
                 PERFORM 3000-FINALIZAR
              END-IF
           END-IF.

           PERFORM 2112-INTEGRIDADE-DATAS.

           PERFORM 2113-MOVER-FIXOS.

           PERFORM 2114-ACESSAR-JOIN-C6-D0.

      *----------------------------------------------------------------*
       2110-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ACESSO AO MODULO MESU9018                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2111-ACESSAR-MESU9018 SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES TO COMU-DADOS-9018.

           INITIALIZE COMU-DADOS-9018.

           MOVE 'VRS001' TO COMU-VERSAO.
           MOVE 237 TO COMU-COD-EMPR.
           MOVE GFCTLH-CAGENCIA TO COMU-COD-DEPDC.
           MOVE GFCTLH-CPAB TO COMU-COD-POSTO.

           MOVE 'MESU9018' TO WRK-MODULO.

           CALL 'POOL0081' USING WRK-MODULO
                                 COMU-DADOS-9018.

           IF RETURN-CODE NOT EQUAL ZEROS
              EVALUATE RETURN-CODE
              WHEN 4
                   MOVE 0518 TO GFCTLI-COD-MSG-ERRO
                   MOVE '0518' TO GFCTG2-COD-MSG
              WHEN 8
                   MOVE 0519 TO GFCTLI-COD-MSG-ERRO
                   MOVE '0519' TO GFCTG2-COD-MSG
              WHEN 12
                   MOVE 0520 TO GFCTLI-COD-MSG-ERRO
                   MOVE '0520' TO GFCTG2-COD-MSG
              END-EVALUATE
              MOVE 1 TO GFCTLI-ERRO
              MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
              PERFORM 1210-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2111-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CONSISTIR DATAS                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2112-INTEGRIDADE-DATAS SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLH-DATA-INICIO(1:2) TO WRK-DIA-AUX.
           MOVE GFCTLH-DATA-INICIO(4:2) TO WRK-MES-AUX.
           MOVE GFCTLH-DATA-INICIO(7:4) TO WRK-ANO-AUX.
           MOVE WRK-DATA-AUX TO WRK-DATA-1470.

           CALL 'POOL1470' USING WRK-DATA-1470.

           IF RETURN-CODE NOT EQUAL ZEROS
              MOVE 1 TO GFCTLI-ERRO
              MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
              MOVE 0786 TO GFCTLI-COD-MSG-ERRO
              MOVE '0786' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

           MOVE GFCTLH-DATA-FIM(1:2) TO WRK-DIA-AUX.
           MOVE GFCTLH-DATA-FIM(4:2) TO WRK-MES-AUX.
           MOVE GFCTLH-DATA-FIM(7:4) TO WRK-ANO-AUX.
           MOVE WRK-DATA-AUX TO WRK-DATA-1470.

           CALL 'POOL1470' USING WRK-DATA-1470.

           IF RETURN-CODE NOT EQUAL ZEROS
              MOVE 1 TO GFCTLI-ERRO
              MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
              MOVE 0787 TO GFCTLI-COD-MSG-ERRO
              MOVE '0787' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2112-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    MOVER CAMPOS FIXOS PARA TABELA GFCTB0C6                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2113-MOVER-FIXOS SECTION.
      *----------------------------------------------------------------*

           INSPECT GFCTLH-DATA-INICIO REPLACING ALL '/' BY '.'.
           INSPECT GFCTLH-DATA-FIM REPLACING ALL '/' BY '.'.

           MOVE GFCTLH-CPACOTE TO CSERVC-TARIF OF GFCTB0C6.
           MOVE GFCTLH-CAGPTO TO CAGPTO-CTA OF GFCTB0C6.
           MOVE GFCTLH-DATA-INICIO TO DINIC-PRMSS-PCOTE OF GFCTB0C6.
           MOVE GFCTLH-DATA-FIM TO DFIM-PRMSS-PCOTE OF GFCTB0C6.

      *----------------------------------------------------------------*
       2113-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ACESSO AS TABELAS GFCTB0C6 E GFCTB0D0                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2114-ACESSAR-JOIN-C6-D0 SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLH-CAGENCIA TO CDEPDC OF GFCTB0D0.
           MOVE GFCTLH-CPAB TO CPOSTO-SERVC OF GFCTB0D0.
BI0810     MOVE GFCTLH-CPSSOA-TARIFA TO CPSSOA-SERVC-TARIF
BI0810        OF GFCTB0C6.

           EXEC SQL
             SELECT
                   A.CSERVC_TARIF,
BI0810              A.CPSSOA_SERVC_TARIF
             INTO
                   :GFCTB0C6.CSERVC-TARIF,
BI0810              :GFCTB0C6.CPSSOA-SERVC-TARIF
             FROM DB2PRD.PRMSS_ADSAO_PCOTE A,
                   DB2PRD.PRMSS_GRP_PAB B
             WHERE A.CSERVC_TARIF = B.CSERVC_TARIF
              AND A.CAGPTO_CTA = B.CAGPTO_CTA
              AND A.DINIC_PRMSS_PCOTE = B.DINIC_PRMSS_PCOTE
              AND A.CSEQ_AGPTO_CTA = B.CSEQ_AGPTO_CTA
              AND A.CSERVC_TARIF = :GFCTB0C6.CSERVC-TARIF
              AND A.CAGPTO_CTA = :GFCTB0C6.CAGPTO-CTA
              AND A.DINIC_PRMSS_PCOTE <= :GFCTB0C6.DINIC-PRMSS-PCOTE
              AND A.DFIM_PRMSS_PCOTE >= :GFCTB0C6.DFIM-PRMSS-PCOTE
BI0810         AND (A.CPSSOA_SERVC_TARIF = :GFCTB0C6.CPSSOA-SERVC-TARIF
BI0810          OR A.CPSSOA_SERVC_TARIF = '3')
              AND B.CDEPDC = :GFCTB0D0.CDEPDC
              AND B.CPOSTO_SERVC = :GFCTB0D0.CPOSTO-SERVC
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0012' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'JOIN-C6-COM-D0' TO GFCT0M-NOME-TAB
              MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
              MOVE '0020' TO GFCT0M-LOCAL
              MOVE 0012 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB0C6' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

           IF SQLCODE EQUAL +100
              MOVE 1 TO GFCTLI-ERRO
              MOVE 0785 TO GFCTLI-COD-MSG-ERRO
              MOVE '0785' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2114-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CONSISTIR AGENCIA                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2120-CONSISTIR-AGENCIA SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES TO COMU-PARM.

           INITIALIZE COMU-PARM.

           MOVE 237 TO COMU-BANCO.
           MOVE GFCTLH-CAGENCIA TO COMU-DEPDC.
           MOVE 'GFCT0132' TO COMU-NOME-PGM.
           MOVE 'MESU9015' TO WRK-MODULO.

           CALL 'POOL0081' USING WRK-MODULO
                                 COMU-PARM
                                 WRK-POOL7100
                                 WRK-SQLCA.

           IF COMU-COD-RETORNO NOT EQUAL ZEROS
              MOVE 1 TO GFCTLI-ERRO
              MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
              MOVE 0465 TO GFCTLI-COD-MSG-ERRO
              MOVE '0465' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

           IF GFCTLH-CAGPTO EQUAL 5
              PERFORM 2112-INTEGRIDADE-DATAS
              PERFORM 2113-MOVER-FIXOS
              PERFORM 2125-ACESSAR-JOIN-C6-C7
           END-IF.

      *----------------------------------------------------------------*
       2120-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ACESSO AO JOIN DAS TABELAS GFCTB0C6 E GFCTB0C7              *
      ******************************************************************
      *----------------------------------------------------------------*
       2125-ACESSAR-JOIN-C6-C7 SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLH-CAGENCIA TO CDEPDC OF GFCTB0C7.
BI0810     MOVE GFCTLH-CPSSOA-TARIFA TO CPSSOA-SERVC-TARIF
BI0810        OF GFCTB0C6.

           EXEC SQL
             SELECT
                   A.CSERVC_TARIF,
BI0810              A.CPSSOA_SERVC_TARIF
             INTO
                   :GFCTB0C6.CSERVC-TARIF,
BI0810              :GFCTB0C6.CPSSOA-SERVC-TARIF
             FROM DB2PRD.PRMSS_ADSAO_PCOTE A,
                   DB2PRD.PRMSS_GRP_DEPDC B
             WHERE A.CSERVC_TARIF = B.CSERVC_TARIF
              AND A.CAGPTO_CTA = B.CAGPTO_CTA
              AND A.DINIC_PRMSS_PCOTE = B.DINIC_PRMSS_PCOTE
              AND A.CSEQ_AGPTO_CTA = B.CSEQ_AGPTO_CTA
              AND A.CSERVC_TARIF = :GFCTB0C6.CSERVC-TARIF
              AND A.CAGPTO_CTA = :GFCTB0C6.CAGPTO-CTA
              AND A.DINIC_PRMSS_PCOTE <= :GFCTB0C6.DINIC-PRMSS-PCOTE
              AND A.DFIM_PRMSS_PCOTE >= :GFCTB0C6.DFIM-PRMSS-PCOTE
BI0810         AND (A.CPSSOA_SERVC_TARIF = :GFCTB0C6.CPSSOA-SERVC-TARIF
BI0810          OR A.CPSSOA_SERVC_TARIF = '3')
              AND B.CDEPDC = :GFCTB0C7.CDEPDC
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0012' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'JOIN-C6-COM-C7' TO GFCT0M-NOME-TAB
              MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
              MOVE '0030' TO GFCT0M-LOCAL
              MOVE 0012 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB0C6' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

           IF SQLCODE EQUAL +100
              MOVE 1 TO GFCTLI-ERRO
              MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
              MOVE 0788 TO GFCTLI-COD-MSG-ERRO
              MOVE '0788' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2125-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CONSISTE EMPRESA                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2130-CONSISTIR-EMPRESA SECTION.
      *----------------------------------------------------------------*

           PERFORM 2120-CONSISTIR-AGENCIA.

           PERFORM 2131-ACESSAR-GFCT5060.

           PERFORM 2112-INTEGRIDADE-DATAS.

           PERFORM 2113-MOVER-FIXOS.

           PERFORM 2132-ACESSAR-JOIN-C6-C8.

      *----------------------------------------------------------------*
       2130-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ACESSAR GFCT5060 PARA CONSISTIR EMPRESA                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2131-ACESSAR-GFCT5060 SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES TO GFCTBR-ENTRADA
                          GFCTBS-SAIDA
                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCTBR-ENTRADA
                      GFCTBS-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE 100 TO GFCTBR-LL.
           MOVE GFCTLH-TRANSACAO TO GFCTBR-TRANSACAO.
           MOVE GFCTLH-FUNCAO TO GFCTBR-FUNCAO.
           MOVE GFCTLH-FUNC-BDSCO TO GFCTBR-FUNC-BDSCO.
           MOVE 'N' TO GFCTBR-FIM.
           MOVE GFCTLH-CAGENCIA TO GFCTBR-COD-DEPDC.
           MOVE GFCTLH-CCONTA TO GFCTBR-CCTA-CLI.

           MOVE 'GFCT5060' TO WRK-MODULO.

           CALL 'POOL0081' USING WRK-MODULO
                                 GFCTBR-ENTRADA
                                 GFCTBS-SAIDA
                                 GFCT0M-AREA-ERROS.

           IF RETURN-CODE NOT EQUAL ZEROS
              MOVE 9 TO WRK-MODULO
              MOVE WRK-MODULO TO WRK-NOME-MOD
                                 GFCT0M-PROGRAMA
              MOVE WRK-MSG01 TO GFCT0M-TEXTO
              MOVE 'APL' TO GFCT0M-TIPO-ACESSO
              MOVE 'GFCT0132' TO GFCT0M-TRANSACAO
              PERFORM 3000-FINALIZAR
           END-IF.

           IF GFCTBS-ERRO NOT EQUAL ZEROS
              MOVE GFCTBS-ERRO TO GFCTLI-ERRO
              MOVE GFCTBS-COD-SQL-ERRO TO GFCTLI-COD-SQL-ERRO
              MOVE GFCTBS-COD-MSG-ERRO TO GFCTLI-COD-MSG-ERRO
              STRING 'GFCT5060-' GFCTBS-DESC-MSG-ERRO
                 DELIMITED BY SIZE INTO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2131-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ACESSO AO JOIN DAS TABELAS GFCTB0C6 E GFCTB0C8              *
      ******************************************************************
      *----------------------------------------------------------------*
       2132-ACESSAR-JOIN-C6-C8 SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLH-CAGENCIA TO CJUNC-DEPDC-DEB OF GFCTB0C8.
           MOVE GFCTLH-CCONTA TO CNRO-CTA-DEB OF GFCTB0C8.
BI0810     MOVE GFCTLH-CPSSOA-TARIFA TO CPSSOA-SERVC-TARIF
BI0810        OF GFCTB0C6.

           EXEC SQL
             SELECT
                   A.CSERVC_TARIF,
BI0810              A.CPSSOA_SERVC_TARIF
             INTO
                   :GFCTB0C6.CSERVC-TARIF,
BI0810              :GFCTB0C6.CPSSOA-SERVC-TARIF
             FROM DB2PRD.PRMSS_ADSAO_PCOTE A,
                   DB2PRD.PRMSS_GRP_EMPR B
             WHERE A.CSERVC_TARIF = B.CSERVC_TARIF
              AND A.CAGPTO_CTA = B.CAGPTO_CTA
              AND A.DINIC_PRMSS_PCOTE = B.DINIC_PRMSS_PCOTE
              AND A.CSEQ_AGPTO_CTA = B.CSEQ_AGPTO_CTA
              AND A.CSERVC_TARIF = :GFCTB0C6.CSERVC-TARIF
              AND A.CAGPTO_CTA = :GFCTB0C6.CAGPTO-CTA
              AND A.DINIC_PRMSS_PCOTE <= :GFCTB0C6.DINIC-PRMSS-PCOTE
              AND A.DFIM_PRMSS_PCOTE >= :GFCTB0C6.DFIM-PRMSS-PCOTE
BI0810         AND (A.CPSSOA_SERVC_TARIF = :GFCTB0C6.CPSSOA-SERVC-TARIF
BI0810          OR A.CPSSOA_SERVC_TARIF = '3')
              AND B.CJUNC_DEPDC_DEB = :GFCTB0C8.CJUNC-DEPDC-DEB
              AND B.CNRO_CTA_DEB = :GFCTB0C8.CNRO-CTA-DEB
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0012' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'JOIN-C6-COM-C8' TO GFCT0M-NOME-TAB
              MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
              MOVE '0040' TO GFCT0M-LOCAL
              MOVE 0012 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB0C6' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

           IF SQLCODE EQUAL +100
              MOVE 1 TO GFCTLI-ERRO
              MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
              MOVE 0789 TO GFCTLI-COD-MSG-ERRO
              MOVE '0789' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2132-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CONSISTE POSTAL                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2140-CONSISTIR-POSTAL-EXP SECTION.
      *----------------------------------------------------------------*

           IF GFCTLH-TOTAL NOT EQUAL 'S'
              PERFORM 2120-CONSISTIR-AGENCIA
              PERFORM 2111-ACESSAR-MESU9018
              IF COMU-COD-TIPO-POSTO NOT EQUAL 9
                 IF GFCTLH-CAGPTO EQUAL 17
                    MOVE 1 TO GFCTLI-ERRO
                    MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
                    MOVE 0521 TO GFCTLI-COD-MSG-ERRO
                    MOVE '0521' TO GFCTG2-COD-MSG
                    PERFORM 1210-OBTER-DESC-MSG
                    MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
                    PERFORM 3000-FINALIZAR
                 ELSE
                    MOVE 1 TO GFCTLI-ERRO
                    MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
                    MOVE 1480 TO GFCTLI-COD-MSG-ERRO
                    MOVE '1480' TO GFCTG2-COD-MSG
                    PERFORM 1210-OBTER-DESC-MSG
                    MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
                    PERFORM 3000-FINALIZAR
                 END-IF
              END-IF
PW1334
PW1334        PERFORM 2145-OBTER-DADOS-YMKCB001
PW1334
           END-IF.

           PERFORM 2112-INTEGRIDADE-DATAS.

           PERFORM 2113-MOVER-FIXOS.

           PERFORM 2141-ACESSAR-JOIN-C6-D1.

      *----------------------------------------------------------------*
       2140-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ACESSO AS TABELAS GFCTB0C6 E GFCTB0D1                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2141-ACESSAR-JOIN-C6-D1 SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLH-CAGENCIA TO CDEPDC OF GFCTB0D1.
           MOVE GFCTLH-CPAB TO CPOSTO-SERVC OF GFCTB0D1.
BI0810     MOVE GFCTLH-CPSSOA-TARIFA TO CPSSOA-SERVC-TARIF
BI0810        OF GFCTB0C6.

           EXEC SQL
             SELECT
                   A.CSERVC_TARIF,
BI0810              A.CPSSOA_SERVC_TARIF
             INTO
                   :GFCTB0C6.CSERVC-TARIF,
BI0810              :GFCTB0C6.CPSSOA-SERVC-TARIF
             FROM DB2PRD.PRMSS_ADSAO_PCOTE A,
                   DB2PRD.PRMSS_GRP_PSTAL B
             WHERE A.CSERVC_TARIF = B.CSERVC_TARIF
              AND A.CAGPTO_CTA = B.CAGPTO_CTA
              AND A.DINIC_PRMSS_PCOTE = B.DINIC_PRMSS_PCOTE
              AND A.CSEQ_AGPTO_CTA = B.CSEQ_AGPTO_CTA
              AND A.CSERVC_TARIF = :GFCTB0C6.CSERVC-TARIF
              AND A.CAGPTO_CTA = :GFCTB0C6.CAGPTO-CTA
              AND A.DINIC_PRMSS_PCOTE <= :GFCTB0C6.DINIC-PRMSS-PCOTE
              AND A.DFIM_PRMSS_PCOTE >= :GFCTB0C6.DFIM-PRMSS-PCOTE
BI0810         AND (A.CPSSOA_SERVC_TARIF = :GFCTB0C6.CPSSOA-SERVC-TARIF
BI0810          OR A.CPSSOA_SERVC_TARIF = '3')
              AND B.CDEPDC = :GFCTB0D1.CDEPDC
              AND B.CPOSTO_SERVC = :GFCTB0D1.CPOSTO-SERVC
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0012' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'JOIN-C6-COM-D1' TO GFCT0M-NOME-TAB
              MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
              MOVE '0050' TO GFCT0M-LOCAL
              MOVE 0012 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB0C6' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

           IF SQLCODE EQUAL +100
              IF GFCTLH-CAGPTO EQUAL 17
                 MOVE 1 TO GFCTLI-ERRO
                 MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
                 MOVE 0790 TO GFCTLI-COD-MSG-ERRO
                 MOVE '0790' TO GFCTG2-COD-MSG
                 PERFORM 1210-OBTER-DESC-MSG
                 MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
                 PERFORM 3000-FINALIZAR
              ELSE
                 MOVE 1 TO GFCTLI-ERRO
                 MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
                 MOVE 1481 TO GFCTLI-COD-MSG-ERRO
                 MOVE '1481' TO GFCTG2-COD-MSG
                 PERFORM 1210-OBTER-DESC-MSG
                 MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
                 PERFORM 3000-FINALIZAR
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       2141-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ACESSO A TABELA YMKCB001                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2145-OBTER-DADOS-YMKCB001 SECTION.
      *----------------------------------------------------------------*

           INITIALIZE KCIMW184-AREA-COMUNICACAO.

           MOVE 01 TO KCIMW184-E-FUNCAO.
           MOVE GFCTLH-CAGENCIA TO KCIMW184-E-CAG-BCRIA-1.
           MOVE GFCTLH-CPAB TO WRK-CPAB-9-05.
           MOVE WRK-CPAB-9-03 TO KCIMW184-E-CPOSTO-BCRIO-1.
           MOVE 'KCIM0184' TO WRK-MODULO.

           CALL WRK-MODULO USING KCIMW184-AREA-COMUNICACAO.

           IF KCIMW184-S-COD-RETORNO NOT EQUAL 00 AND 98
              MOVE 1 TO GFCTLI-ERRO
              MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
              MOVE 1875 TO GFCTLI-COD-MSG-ERRO
              MOVE '1875' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.
           IF KCIMW184-S-COD-RETORNO EQUAL 98
              IF GFCTLH-CAGPTO EQUAL 17
                 MOVE 1 TO GFCTLI-ERRO
                 MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
                 MOVE 0521 TO GFCTLI-COD-MSG-ERRO
                 MOVE '0521' TO GFCTG2-COD-MSG
                 PERFORM 1210-OBTER-DESC-MSG
                 MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
                 PERFORM 3000-FINALIZAR
              ELSE
                 MOVE 1 TO GFCTLI-ERRO
                 MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
                 MOVE 1480 TO GFCTLI-COD-MSG-ERRO
                 MOVE '1480' TO GFCTG2-COD-MSG
                 PERFORM 1210-OBTER-DESC-MSG
                 MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
                 PERFORM 3000-FINALIZAR
              END-IF
           END-IF.

           IF KCIMW184-S-CEMPR-CORSP(1) NOT EQUAL 10000 AND
              GFCTLH-CAGPTO EQUAL 17
              MOVE 1 TO GFCTLI-ERRO
              MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
              MOVE 0521 TO GFCTLI-COD-MSG-ERRO
              MOVE '0521' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

           IF KCIMW184-S-CEMPR-CORSP(1) EQUAL 10000 AND
              GFCTLH-CAGPTO EQUAL 20
              MOVE 1 TO GFCTLI-ERRO
              MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
              MOVE 1480 TO GFCTLI-COD-MSG-ERRO
              MOVE '1480' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2145-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CONSISTE CLIENTE                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2150-CONSISTIR-CLIENTE SECTION.
      *----------------------------------------------------------------*

ST2506*    M0VE SPACES TO WRK-CONTROLE-0110.
ST2506*    M0VE GFCTLH-CGC-CPF TO WRK-CPF-0110.
ST2506*    M0VE GFCTLH-FILIAL TO WRK-FILIAL-0110.
ST2506*
ST2506*    CA1L 'POOL0110' USING WRK-CGC-CPF-0110
ST2506*                          WRK-CONTROLE-0110.
           
ST2506     MOVE ZEROS          TO WRK-CAD-CONTROLE. 
ST2506     MOVE GFCTLH-CGC-CPF TO WRK-CAD-NUMERO.
ST2506     MOVE GFCTLH-FILIAL  TO WRK-CAD-FILIAL.
ST2506
ST2506     CALL WRK-BRAD2000     USING WRK-CAD-CPFCGC
ST2506                                 WRK-CAD-CONTROLE.

           IF RETURN-CODE NOT EQUAL ZEROS
              MOVE 1 TO GFCTLI-ERRO
              MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
              MOVE 0468 TO GFCTLI-COD-MSG-ERRO
              MOVE '0468' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

ST2506     MOVE WRK-CAD-CONTROLE TO WRK-9-02
ST2506*    IF GFCTLH-CONTROLE NOT EQUAL WRK-CONTROLE-0110
           IF GFCTLH-CONTROLE NOT EQUAL WRK-9-02-X 
              MOVE 1 TO GFCTLI-ERRO
              MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
              MOVE 0507 TO GFCTLI-COD-MSG-ERRO
              MOVE '0507' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2151-ACESSAR-GFCT5110.

           PERFORM 2112-INTEGRIDADE-DATAS.

           PERFORM 2113-MOVER-FIXOS.

           PERFORM 2152-ACESSAR-JOIN-C6-H9.

      *----------------------------------------------------------------*
       2150-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ACESSAR GFCT5110 PARA PESQUISAR BASE CLIENTE                *
      ******************************************************************
      *----------------------------------------------------------------*
       2151-ACESSAR-GFCT5110 SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES TO GFCTIU-ENTRADA
                          GFCTIV-SAIDA
                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCTIU-ENTRADA
                      GFCTIV-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE 100 TO GFCTIU-LL.
           MOVE GFCTLH-TRANSACAO TO GFCTIU-TRANSACAO.
           MOVE GFCTLH-FUNCAO TO GFCTIU-FUNCAO.
           MOVE GFCTLH-FUNC-BDSCO TO GFCTIU-FUNC-BDSCO.
           MOVE 'N' TO GFCTIU-FIM.

      * TODO - AJUSTAR GFCT5110 PARA USAR GFCTIU     
ST2506*    MOVE GFCTLH-CGC-CPF  TO GFCTIU-NRO-CPF-CNPJ.
ST2506*    MOVE GFCTLH-FILIAL   TO GFCTIU-NRO-FILIAL.
ST2506     MOVE GFCTLH-CGC-CPF  TO GFCTIU-COD-CPF-CNPJ.
ST2506     MOVE GFCTLH-FILIAL   TO GFCTIU-COD-FILIAL.
           MOVE GFCTLH-CONTROLE TO GFCTIU-CTRL-CPF-CNPJ.
           MOVE 'GFCT5110' TO WRK-MODULO.

           CALL 'POOL0081' USING WRK-MODULO
                                 GFCTIU-ENTRADA
                                 GFCTIV-SAIDA
                                 GFCT0M-AREA-ERROS.

           IF RETURN-CODE NOT EQUAL ZEROS
              MOVE 9 TO WRK-MODULO
              MOVE WRK-MODULO TO WRK-NOME-MOD
                                 GFCT0M-PROGRAMA
              MOVE WRK-MSG01 TO GFCT0M-TEXTO
              MOVE 'APL' TO GFCT0M-TIPO-ACESSO
              MOVE 'GFCT0132' TO GFCT0M-TRANSACAO
              PERFORM 3000-FINALIZAR
           END-IF.

           IF GFCTIV-ERRO NOT EQUAL ZEROS
              MOVE GFCTIV-ERRO TO GFCTLI-ERRO
              MOVE GFCTIV-COD-SQL-ERRO TO GFCTLI-COD-SQL-ERRO
              MOVE GFCTIV-COD-MSG-ERRO TO GFCTLI-COD-MSG-ERRO
              STRING 'GFCT5110-' GFCTIV-DESC-MSG-ERRO
                 DELIMITED BY SIZE INTO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2151-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ACESSO AS TABELAS GFCTB0C6 E GFCTB0H9                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2152-ACESSAR-JOIN-C6-H9 SECTION.
      *----------------------------------------------------------------*

ST2506*    M0VE GFCTLH-CGC-CPF TO CCGC-CPF OF GFCTB0H9.
ST2506     MOVE GFCTLH-CGC-CPF TO CCGC-CPF-ST OF GFCTB0H9.
           MOVE GFCTLH-CPSSOA-TARIFA TO CPSSOA-SERVC-TARIF
              OF GFCTB0C6.

           EXEC SQL
             SELECT
                   A.CSERVC_TARIF,
BI0810              A.CPSSOA_SERVC_TARIF
             INTO
                   :GFCTB0C6.CSERVC-TARIF,
BI0810              :GFCTB0C6.CPSSOA-SERVC-TARIF
             FROM DB2PRD.PRMSS_ADSAO_PCOTE A,
                   DB2PRD.TPRMSS_GRP_CLI B
             WHERE A.CSERVC_TARIF = B.CSERVC_TARIF
              AND A.CAGPTO_CTA = B.CAGPTO_CTA
              AND A.DINIC_PRMSS_PCOTE = B.DINIC_PRMSS_PCOTE
              AND A.CSEQ_AGPTO_CTA = B.CSEQ_AGPTO_CTA
              AND A.CSERVC_TARIF = :GFCTB0C6.CSERVC-TARIF
              AND A.CAGPTO_CTA = :GFCTB0C6.CAGPTO-CTA
              AND A.DINIC_PRMSS_PCOTE <= :GFCTB0C6.DINIC-PRMSS-PCOTE
              AND A.DFIM_PRMSS_PCOTE >= :GFCTB0C6.DFIM-PRMSS-PCOTE
BI0810         AND (A.CPSSOA_SERVC_TARIF = :GFCTB0C6.CPSSOA-SERVC-TARIF
BI0810          OR A.CPSSOA_SERVC_TARIF = '3')
ST2605*       AND B.CCGC_CPF = :GFCTB0H9.CCGC-CPF
ST2605        AND B.CCGC_CPF_ST = :GFCTB0H9.CCGC-CPF-ST
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0012' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'JOIN-C6-COM-H9' TO GFCT0M-NOME-TAB
              MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
              MOVE '0070' TO GFCT0M-LOCAL
              MOVE 0012 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB0C6' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

           IF SQLCODE EQUAL +100
              MOVE 1 TO GFCTLI-ERRO
              MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
              MOVE 0791 TO GFCTLI-COD-MSG-ERRO
              MOVE '0791' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2152-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CONSISTE SEGMENTO                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2160-CONSISTIR-SEGMENTO SECTION.
      *----------------------------------------------------------------*

           PERFORM 2112-INTEGRIDADE-DATAS.

           PERFORM 2113-MOVER-FIXOS.

           PERFORM 2161-ACESSAR-JOIN-C6-D2.

      *----------------------------------------------------------------*
       2160-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ACESSO AS TABELAS GFCTB0C6 E GFCTB0D2                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2161-ACESSAR-JOIN-C6-D2 SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLH-CSEGMENTO TO CSGMTO-GSTAO-TARIF OF GFCTB0D2.
BI0810     MOVE GFCTLH-CPSSOA-TARIFA TO CPSSOA-SERVC-TARIF
BI0810        OF GFCTB0C6.

           EXEC SQL
             SELECT
                   A.CSERVC_TARIF,
BI0810              A.CPSSOA_SERVC_TARIF
             INTO
                   :GFCTB0C6.CSERVC-TARIF,
BI0810              :GFCTB0C6.CPSSOA-SERVC-TARIF
             FROM DB2PRD.PRMSS_ADSAO_PCOTE A,
                   DB2PRD.PRMSS_GRP_SGMTO B
             WHERE A.CSERVC_TARIF = B.CSERVC_TARIF
              AND A.CAGPTO_CTA = B.CAGPTO_CTA
              AND A.DINIC_PRMSS_PCOTE = B.DINIC_PRMSS_PCOTE
              AND A.CSEQ_AGPTO_CTA = B.CSEQ_AGPTO_CTA
              AND A.CSERVC_TARIF = :GFCTB0C6.CSERVC-TARIF
              AND A.CAGPTO_CTA = :GFCTB0C6.CAGPTO-CTA
              AND A.DINIC_PRMSS_PCOTE <= :GFCTB0C6.DINIC-PRMSS-PCOTE
              AND A.DFIM_PRMSS_PCOTE >= :GFCTB0C6.DFIM-PRMSS-PCOTE
BI0810         AND (A.CPSSOA_SERVC_TARIF = :GFCTB0C6.CPSSOA-SERVC-TARIF
BI0810          OR A.CPSSOA_SERVC_TARIF = '3')
              AND B.CSGMTO_GSTAO_TARIF = :GFCTB0D2.CSGMTO-GSTAO-TARIF
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0012' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'JOIN-C6-COM-D2' TO GFCT0M-NOME-TAB
              MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
              MOVE '0080' TO GFCT0M-LOCAL
              MOVE 0012 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB0C6' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

           IF SQLCODE EQUAL +100
              MOVE 1 TO GFCTLI-ERRO
              MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
              MOVE 0792 TO GFCTLI-COD-MSG-ERRO
              MOVE '0792' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2161-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CONSISTE MUNICIPIO                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2170-CONSISTIR-MUNICIPIO SECTION.
      *----------------------------------------------------------------*

           PERFORM 2171-ACESSAR-RURC9020.

           PERFORM 2112-INTEGRIDADE-DATAS.

           PERFORM 2113-MOVER-FIXOS.

           PERFORM 2172-ACESSAR-JOIN-C6-C9.

      *----------------------------------------------------------------*
       2170-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ACESSA MODULO RURC9020                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2171-ACESSAR-RURC9020 SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES TO AREA-RURC88.

           INITIALIZE AREA-RURC88.

           MOVE GFCTLH-CMUNIC TO COD-MUNIC-RURC88.
           MOVE 'RURC9020' TO WRK-MODULO.

           CALL 'POOL0081' USING WRK-MODULO
                                 AREA-RURC88.

           IF RETURN-CODE NOT EQUAL ZEROS
              MOVE 1 TO GFCTLI-ERRO
              MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
              MOVE 0732 TO GFCTLI-COD-MSG-ERRO
              MOVE '0732' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

           IF COD-RETORNO-RURC88 NOT EQUAL ZEROS
              EVALUATE COD-RETORNO-RURC88
              WHEN 10
                   MOVE 0675 TO GFCTLI-COD-MSG-ERRO
                   MOVE '0675' TO GFCTG2-COD-MSG
                   MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
              WHEN 20
                   MOVE 0733 TO GFCTLI-COD-MSG-ERRO
                   MOVE '0733' TO GFCTG2-COD-MSG
                   MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
              WHEN 99
                   MOVE 0734 TO GFCTLI-COD-MSG-ERRO
                   MOVE '0734' TO GFCTG2-COD-MSG
                   MOVE SQLCODE-RURC88
                      TO GFCTLI-COD-SQL-ERRO
              END-EVALUATE
              MOVE 1 TO GFCTLI-ERRO
              PERFORM 1210-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2171-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ACESSO AS TABELAS GFCTB0C6 E GFCTB0C9                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2172-ACESSAR-JOIN-C6-C9 SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLH-CMUNIC TO CMUN-IBGE OF GFCTB0C9.
BI0810     MOVE GFCTLH-CPSSOA-TARIFA TO CPSSOA-SERVC-TARIF
BI0810        OF GFCTB0C6.

           EXEC SQL
             SELECT
                   A.CSERVC_TARIF,
BI0810              A.CPSSOA_SERVC_TARIF
             INTO
                   :GFCTB0C6.CSERVC-TARIF,
BI0810              :GFCTB0C6.CPSSOA-SERVC-TARIF
             FROM DB2PRD.PRMSS_ADSAO_PCOTE A,
                   DB2PRD.PRMSS_GRP_MUN B
             WHERE A.CSERVC_TARIF = B.CSERVC_TARIF
              AND A.CAGPTO_CTA = B.CAGPTO_CTA
              AND A.DINIC_PRMSS_PCOTE = B.DINIC_PRMSS_PCOTE
              AND A.CSEQ_AGPTO_CTA = B.CSEQ_AGPTO_CTA
              AND A.CSERVC_TARIF = :GFCTB0C6.CSERVC-TARIF
              AND A.CAGPTO_CTA = :GFCTB0C6.CAGPTO-CTA
              AND A.DINIC_PRMSS_PCOTE <= :GFCTB0C6.DINIC-PRMSS-PCOTE
              AND A.DFIM_PRMSS_PCOTE >= :GFCTB0C6.DFIM-PRMSS-PCOTE
BI0810         AND (A.CPSSOA_SERVC_TARIF = :GFCTB0C6.CPSSOA-SERVC-TARIF
BI0810          OR A.CPSSOA_SERVC_TARIF = '3')
              AND B.CMUN_IBGE = :GFCTB0C9.CMUN-IBGE
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0012' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'JOIN-C6-COM-C9' TO GFCT0M-NOME-TAB
              MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
              MOVE '0090' TO GFCT0M-LOCAL
              MOVE 0012 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB0C6' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

           IF SQLCODE EQUAL +100
              MOVE 1 TO GFCTLI-ERRO
              MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
              MOVE 0793 TO GFCTLI-COD-MSG-ERRO
              MOVE '0793' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2172-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CONSISTE UF                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2180-CONSISTIR-UF SECTION.
      *----------------------------------------------------------------*

           PERFORM 2181-ACESSAR-DCIT8000.

           PERFORM 2112-INTEGRIDADE-DATAS.

           PERFORM 2113-MOVER-FIXOS.

           PERFORM 2182-ACESSAR-JOIN-C6-D3.

      *----------------------------------------------------------------*
       2180-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ACESSA MODULO DCIT8000                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2181-ACESSAR-DCIT8000 SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES TO WRK-DCIT-UNIDADE-FEDERATIVA.
           INITIALIZE WRK-DCIT-UNIDADE-FEDERATIVA.

           MOVE GFCTLH-CUF TO CSGL-UF OF DCITV002.
           MOVE DCITV002 TO WRK-AREA-DCLGEN-DCITV002.
           MOVE 'DCIT8000' TO WRK-MODULO.

           CALL 'POOL0081' USING WRK-MODULO
                                 WRK-DCIT-UNIDADE-FEDERATIVA
                                 WRK-POOL7100-DCIT
                                 WRK-SQLCA-DCIT.

           IF RETURN-CODE NOT EQUAL ZEROS
              MOVE 1 TO GFCTLI-ERRO
              MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
              MOVE 0735 TO GFCTLI-COD-MSG-ERRO
              MOVE '0735' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

           IF WRK-DCIT-CODIGO-RETORNO NOT EQUAL ZEROS
              EVALUATE WRK-DCIT-CODIGO-RETORNO
              WHEN 1
                   MOVE 0736 TO GFCTLI-COD-MSG-ERRO
                   MOVE '0736' TO GFCTG2-COD-MSG
              WHEN 2
                   MOVE 0737 TO GFCTLI-COD-MSG-ERRO
                   MOVE '0737' TO GFCTG2-COD-MSG
              WHEN 3
                   MOVE 0738 TO GFCTLI-COD-MSG-ERRO
                   MOVE '0738' TO GFCTG2-COD-MSG
              WHEN 4
                   INITIALIZE GFCT0M-ERRO-SQL
                   PERFORM 1210-OBTER-DESC-MSG
                   MOVE ' - DCITV002' TO WRK-NOME-TAB
                   MOVE 'V01_UF' TO GFCT0M-NOME-TAB
                   MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
                   MOVE '0100' TO GFCT0M-LOCAL
                   MOVE 0005 TO GFCTLI-COD-MSG-ERRO
                   MOVE '0005' TO GFCTG2-COD-MSG
                   MOVE 9 TO GFCTLI-ERRO
                   MOVE 'GFCT0132' TO GFCT0M-PROGRAMA
                   MOVE 'DB2' TO GFCT0M-TIPO-ACESSO
                   MOVE WRK-SQLCA TO SQLCA
                                     GFCT0M-SQLCA-AREA
                   MOVE SQLCODE TO GFCT0M-SQLCODE
                                   GFCTLI-COD-SQL-ERRO
                   MOVE SPACES TO GFCTLI-DESC-MSG-ERRO
                   STRING GFCTG3-DESC-MSG WRK-NOME-TAB
                      DELIMITED BY '  ' INTO GFCTLI-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
              END-EVALUATE

              MOVE 1 TO GFCTLI-ERRO
              MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
              PERFORM 1210-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2181-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ACESSO AS TABELAS GFCTB0C6 E GFCTB0D3                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2182-ACESSAR-JOIN-C6-D3 SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLH-CUF TO CSGL-UF OF GFCTB0D3.
BI0810     MOVE GFCTLH-CPSSOA-TARIFA TO CPSSOA-SERVC-TARIF
BI0810        OF GFCTB0C6.

           EXEC SQL
             SELECT
                   A.CSERVC_TARIF,
BI0810              A.CPSSOA_SERVC_TARIF
             INTO
                   :GFCTB0C6.CSERVC-TARIF,
BI0810              :GFCTB0C6.CPSSOA-SERVC-TARIF
             FROM DB2PRD.PRMSS_ADSAO_PCOTE A,
                   DB2PRD.PRMSS_GRP_UF B
             WHERE A.CSERVC_TARIF = B.CSERVC_TARIF
              AND A.CAGPTO_CTA = B.CAGPTO_CTA
              AND A.DINIC_PRMSS_PCOTE = B.DINIC_PRMSS_PCOTE
              AND A.CSEQ_AGPTO_CTA = B.CSEQ_AGPTO_CTA
              AND A.CSERVC_TARIF = :GFCTB0C6.CSERVC-TARIF
              AND A.CAGPTO_CTA = :GFCTB0C6.CAGPTO-CTA
              AND A.DINIC_PRMSS_PCOTE <= :GFCTB0C6.DINIC-PRMSS-PCOTE
              AND A.DFIM_PRMSS_PCOTE >= :GFCTB0C6.DFIM-PRMSS-PCOTE
BI0810         AND (A.CPSSOA_SERVC_TARIF = :GFCTB0C6.CPSSOA-SERVC-TARIF
BI0810          OR A.CPSSOA_SERVC_TARIF = '3')
              AND B.CSGL_UF = :GFCTB0D3.CSGL-UF
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0012' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'JOIN-C6-COM-D3' TO GFCT0M-NOME-TAB
              MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
              MOVE '0110' TO GFCT0M-LOCAL
              MOVE 0012 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB0C6' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

           IF SQLCODE EQUAL +100
              MOVE 1 TO GFCTLI-ERRO
              MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
              MOVE 0794 TO GFCTLI-COD-MSG-ERRO
              MOVE '0794' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2182-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CONSISTENCIA DE DATAS                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-CONSISTIR-DATAS SECTION.
      *----------------------------------------------------------------*

           STRING GFCTLH-DATA-INICIO(07:04)
                  GFCTLH-DATA-INICIO(04:02)
                  GFCTLH-DATA-INICIO(01:02)
              DELIMITED BY SIZE INTO WRK-DINIC.

           STRING GFCTLH-DATA-FIM(07:04)
                  GFCTLH-DATA-FIM(04:02)
                  GFCTLH-DATA-FIM(01:02)
              DELIMITED BY SIZE INTO WRK-DFIM.

           STRING GFCTLH-DATA-INICIO-PCTE(07:04)
                  GFCTLH-DATA-INICIO-PCTE(04:02)
                  GFCTLH-DATA-INICIO-PCTE(01:02)
              DELIMITED BY SIZE INTO WRK-DINIC-PCOTE.

           STRING GFCTLH-DATA-FIM-PCTE(07:04)
                  GFCTLH-DATA-FIM-PCTE(04:02)
                  GFCTLH-DATA-FIM-PCTE(01:02)
              DELIMITED BY SIZE INTO WRK-DFIM-PCOTE.

           STRING DPROCM-ATUAL OF GFCTB0A1(07:04)
                  DPROCM-ATUAL OF GFCTB0A1(04:02)
                  DPROCM-ATUAL OF GFCTB0A1(01:02)
              DELIMITED BY SIZE INTO WRK-DPROCM.

           IF WRK-DINIC NOT GREATER WRK-DPROCM
              MOVE 1 TO GFCTLI-ERRO
              MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
              MOVE 0746 TO GFCTLI-COD-MSG-ERRO
              MOVE '0746' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

           IF WRK-DFIM LESS WRK-DINIC
              MOVE 1 TO GFCTLI-ERRO
              MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
              MOVE 0747 TO GFCTLI-COD-MSG-ERRO
              MOVE '0747' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

           IF WRK-DINIC LESS WRK-DINIC-PCOTE
              MOVE 1 TO GFCTLI-ERRO
              MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
              MOVE 0748 TO GFCTLI-COD-MSG-ERRO
              MOVE '0748' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

           IF WRK-DFIM GREATER WRK-DFIM-PCOTE
              MOVE 1 TO GFCTLI-ERRO
              MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
              MOVE 0749 TO GFCTLI-COD-MSG-ERRO
              MOVE '0749' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

           IF GFCTLH-DATA-INICIO(01:02) NOT EQUAL 01
              MOVE 1 TO GFCTLI-ERRO
              MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
              MOVE 0750 TO GFCTLI-COD-MSG-ERRO
              MOVE '0750' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2210-VERIFICAR-ULTIMO-DIA.

      *----------------------------------------------------------------*
       2200-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA SE GFCTLH-DATA-FIM EH ULTIMO DIA DO MES            *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-VERIFICAR-ULTIMO-DIA SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-DFIM TO WRK-DATA-01-ENT.
           MOVE '01' TO WRK-DATA-01-ENT(7:2).
           MOVE WRK-DATA-01-R TO WRK-1640-DATA-ENTRADA.
           MOVE 4 TO WRK-1640-FORMATO-ENTRADA
                     WRK-1640-FORMATO-SAIDA.
           MOVE 'M' TO WRK-1640-TIPO-INCREMENTO.
           MOVE 1 TO WRK-1640-INCREMENTO.

           CALL 'POOL1640' USING WRK-1640-DATA-ENTRADA
                                 WRK-1640-FORMATO-ENTRADA
                                 WRK-1640-TIPO-INCREMENTO
                                 WRK-1640-INCREMENTO
                                 WRK-1640-FORMATO-SAIDA
                                 WRK-1640-DATA-SAIDA
                                 WRK-1640-MENSAGEM.

           IF RETURN-CODE NOT EQUAL ZEROS
              MOVE 1 TO GFCTLI-ERRO
              MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
              MOVE 0767 TO GFCTLI-COD-MSG-ERRO
              MOVE '0767' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

           MOVE WRK-1640-DATA-SAIDA(3:8) TO WRK-DATA-0025.
           MOVE WRK-DATA-0025 TO WRK-DATA-ABERT.
           MOVE -1 TO WRK-PRAZO.

           CALL 'POOL0025' USING WRK-DATA-ABERT
                                 WRK-PRAZO
                                 WRK-DATA-VENC.

           IF RETURN-CODE NOT EQUAL ZEROS
              MOVE 1 TO GFCTLI-ERRO
              MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
              MOVE 0767 TO GFCTLI-COD-MSG-ERRO
              MOVE '0767' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

           MOVE WRK-DATA-VENC TO WRK-DATA-0025.

           IF WRK-DATA-0025(07:02) NOT EQUAL WRK-DFIM(07:02)
              MOVE 1 TO GFCTLI-ERRO
              MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
              MOVE 0751 TO GFCTLI-COD-MSG-ERRO
              MOVE '0751' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2210-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATAMENTO DO CURSOR DA TABELA GFCTB001                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-VERIF-DUPLICIDADE SECTION.
      *----------------------------------------------------------------*

           MOVE 'N' TO WRK-FIM-CSR01-GFCTB001
                       WRK-ACHOU-DUPLICIDADE.

           PERFORM 2310-OPEN-CSR01-GFCTB001.

           PERFORM 2320-FETCH-CSR01-GFCTB001.

           PERFORM 2330-PROCESSAR-CSR01-GFCTB001 UNTIL
              WRK-FIM-CSR01-GFCTB001 EQUAL 'S' OR
              WRK-ACHOU-DUPLICIDADE EQUAL 'S'.

           PERFORM 2340-CLOSE-CSR01-GFCTB001.

           IF WRK-ACHOU-DUPLICIDADE EQUAL 'S'
              MOVE 1 TO GFCTLI-ERRO
              MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
              MOVE 0756 TO GFCTLI-COD-MSG-ERRO
              MOVE '0756' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ABERTURA DO CURSOR DA TABELA GFCTB001                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2310-OPEN-CSR01-GFCTB001 SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLH-CPACOTE TO CSERVC-TARIF OF GFCTB001.
           MOVE GFCTLH-CAGPTO TO CAGPTO-CTA OF GFCTB001.
           MOVE GFCTLH-DATA-INICIO TO DFIM-ADSAO-COMP OF GFCTB001.

           EXEC SQL
                       OPEN CSR01-GFCTB001
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0005' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'ADSAO_COMP_PCOTE' TO GFCT0M-NOME-TAB
              MOVE 'OPEN' TO GFCT0M-COMANDO-SQL
              MOVE '0120' TO GFCT0M-LOCAL
              MOVE 0005 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB001' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2310-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LEITURA DO CURSOR DA TABELA GFCTB001                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2320-FETCH-CSR01-GFCTB001 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR01-GFCTB001 INTO
                   :GFCTB001.CSERVC-TARIF,
                   :GFCTB001.CAGPTO-CTA,
                   :GFCTB001.CSEQ-AGPTO-CTA,
                   :GFCTB001.DINIC-ADSAO-COMP,
BI0810              :GFCTB001.CPSSOA-SERVC-TARIF
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0006' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'ADSAO_COMP_PCOTE' TO GFCT0M-NOME-TAB
              MOVE 'FETCH' TO GFCT0M-COMANDO-SQL
              MOVE '0130' TO GFCT0M-LOCAL
              MOVE 0006 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB001' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

           IF SQLCODE EQUAL +100
              MOVE 'S' TO WRK-FIM-CSR01-GFCTB001
           END-IF.

      *----------------------------------------------------------------*
       2320-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA DUPLICIDADE                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2330-PROCESSAR-CSR01-GFCTB001 SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTLH-CAGPTO
           WHEN 14
                PERFORM 2331-VERIFICAR-AGENCIA
           WHEN 4
                PERFORM 2332-VERIFICAR-CLIENTE
           WHEN 16
                PERFORM 2333-VERIFICAR-EMPRESA
           WHEN 15
                PERFORM 2334-VERIFICAR-POSTO
           WHEN 17
                PERFORM 2335-VERIFICAR-POSTAL-EXP
           WHEN 20
                PERFORM 2335-VERIFICAR-POSTAL-EXP
           WHEN 9
                PERFORM 2336-VERIFICAR-SEGMENTO
           WHEN 18
                PERFORM 2337-VERIFICAR-MUNICIPIO
           WHEN 19
                PERFORM 2338-VERIFICAR-UF
           END-EVALUATE.

           PERFORM 2320-FETCH-CSR01-GFCTB001.

      *----------------------------------------------------------------*
       2330-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA EXISTENCIA DA AGENCIA NA TABELA GFCTB002           *
      ******************************************************************
      *----------------------------------------------------------------*
       2331-VERIFICAR-AGENCIA SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB001 TO CAGPTO-CTA OF GFCTB002.
           MOVE CSERVC-TARIF OF GFCTB001 TO CSERVC-TARIF
              OF GFCTB002.
           MOVE DPROCM-ATUAL OF GFCTB0A1 TO DINIC-ADSAO-COMP
              OF GFCTB002.
           MOVE 237 TO CEMPR-INC OF GFCTB002.

           IF GFCTLH-TOTAL EQUAL 'S'
              MOVE ZEROS TO CDEPDC OF GFCTB002
           ELSE
              MOVE GFCTLH-CAGENCIA TO CDEPDC OF GFCTB002
           END-IF.

           EXEC SQL
             SELECT
                   CEMPR_INC,
                   CDEPDC
             INTO
                   :GFCTB002.CEMPR-INC,
                   :GFCTB002.CDEPDC
             FROM DB2PRD.ADSAO_GRP_DEPDC
             WHERE CAGPTO_CTA = :GFCTB002.CAGPTO-CTA
              AND DINIC_ADSAO_COMP <= :GFCTB002.DINIC-ADSAO-COMP
              AND CEMPR_INC = :GFCTB002.CEMPR-INC
              AND CDEPDC = :GFCTB002.CDEPDC
           END-EXEC.

BI0810     IF (SQLCODE NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0012' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'ADSAO_GRP_DEPDC' TO GFCT0M-NOME-TAB
              MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
              MOVE '0140' TO GFCT0M-LOCAL
              MOVE 0012 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB002' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

BI0810     IF SQLCODE EQUAL ZEROS OR -811
BI0810        PERFORM 2500-VERIFICA-DUPLICADO
              GO TO 2331-99-FIM
           END-IF.

           MOVE CAGPTO-CTA OF GFCTB001 TO CAGPTO-CTA OF GFCTB002.
           MOVE CSERVC-TARIF OF GFCTB001 TO CSERVC-TARIF
              OF GFCTB002.
           MOVE DPROCM-ATUAL OF GFCTB0A1 TO DINIC-ADSAO-COMP
              OF GFCTB002.
           MOVE 237 TO CEMPR-INC OF GFCTB002.
           MOVE ZEROS TO CDEPDC OF GFCTB002.

           IF GFCTLH-TOTAL EQUAL 'S'
              EXEC SQL
               SELECT
                   CEMPR_INC,
                   CDEPDC
               INTO
                   :GFCTB002.CEMPR-INC,
                   :GFCTB002.CDEPDC
               FROM DB2PRD.ADSAO_GRP_DEPDC
               WHERE CAGPTO_CTA = :GFCTB002.CAGPTO-CTA
               AND DINIC_ADSAO_COMP <= :GFCTB002.DINIC-ADSAO-COMP
              END-EXEC

              IF (SQLCODE NOT EQUAL ZEROS AND +100 AND -811) OR
                 (SQLWARN0 EQUAL 'W')
                 MOVE SPACES TO GFCT0M-AREA-ERROS
                 INITIALIZE GFCT0M-AREA-ERROS
                 MOVE '0012' TO GFCTG2-COD-MSG
                 PERFORM 1210-OBTER-DESC-MSG
                 MOVE 'ADSAO_GRP_DEPDC' TO GFCT0M-NOME-TAB
                 MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
                 MOVE '0150' TO GFCT0M-LOCAL
                 MOVE 0012 TO GFCTLI-COD-MSG-ERRO
                 MOVE ' - GFCTB002' TO WRK-NOME-TAB
                 PERFORM 1321-MOVER-ERRO-SQL
                 MOVE SPACES TO GFCTLI-DADOS-RST
                 PERFORM 3000-FINALIZAR
              END-IF
              IF (SQLCODE EQUAL ZEROS) OR
                 (SQLCODE EQUAL -811)
                 MOVE 1 TO GFCTLI-ERRO
                 MOVE 968 TO GFCTLI-COD-MSG-ERRO
                 MOVE '0968' TO GFCTG2-COD-MSG
                 MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
                 PERFORM 1210-OBTER-DESC-MSG
                 MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
                 PERFORM 3000-FINALIZAR
              END-IF
           ELSE
              EXEC SQL
               SELECT
                   CEMPR_INC,
                   CDEPDC
               INTO
                   :GFCTB002.CEMPR-INC,
                   :GFCTB002.CDEPDC
               FROM DB2PRD.ADSAO_GRP_DEPDC
               WHERE CAGPTO_CTA = :GFCTB002.CAGPTO-CTA
               AND DINIC_ADSAO_COMP <= :GFCTB002.DINIC-ADSAO-COMP
               AND CEMPR_INC = :GFCTB002.CEMPR-INC
               AND CDEPDC = :GFCTB002.CDEPDC
              END-EXEC
BI0810        IF (SQLCODE NOT EQUAL ZEROS AND +100 AND -811) OR
                 (SQLWARN0 EQUAL 'W')
                 MOVE SPACES TO GFCT0M-AREA-ERROS
                 INITIALIZE GFCT0M-AREA-ERROS
                 MOVE '0012' TO GFCTG2-COD-MSG
                 PERFORM 1210-OBTER-DESC-MSG
                 MOVE 'ADSAO_GRP_DEPDC' TO GFCT0M-NOME-TAB
                 MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
                 MOVE '0160' TO GFCT0M-LOCAL
                 MOVE 0012 TO GFCTLI-COD-MSG-ERRO
                 MOVE ' - GFCTB002' TO WRK-NOME-TAB
                 PERFORM 1321-MOVER-ERRO-SQL
                 MOVE SPACES TO GFCTLI-DADOS-RST
                 PERFORM 3000-FINALIZAR
              END-IF
              IF (SQLCODE EQUAL ZEROS) OR
BI0810           (SQLCODE EQUAL -811)
                 MOVE 1 TO GFCTLI-ERRO
                 MOVE 969 TO GFCTLI-COD-MSG-ERRO
                 MOVE '0969' TO GFCTG2-COD-MSG
                 MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
                 PERFORM 1210-OBTER-DESC-MSG
                 MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
                 PERFORM 3000-FINALIZAR
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       2331-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA EXISTENCIA DO CLIENTE NA TABELA GFCTB0H9           *
      ******************************************************************
      *----------------------------------------------------------------*
       2332-VERIFICAR-CLIENTE SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB001 TO CAGPTO-CTA OF GFCTB0H9.
           MOVE CSERVC-TARIF OF GFCTB001 TO CSERVC-TARIF OF GFCTB0H9.
           MOVE DPROCM-ATUAL OF GFCTB0A1 TO DINIC-ADSAO-COMP
              OF GFCTB0H9.
ST2506*    M0VE GFCTLH-CGC-CPF TO CCGC-CPF OF GFCTB0H9.
ST2506*    M0VE GFCTLH-FILIAL TO CFLIAL-CGC OF GFCTB0H9.
ST2506*    M0VE GFCTLH-CONTROLE TO CCTRL-CPF-CGC OF GFCTB0H9.
ST2506     MOVE GFCTLH-CGC-CPF TO CCGC-CPF-ST OF GFCTB0H9.
ST2506     MOVE GFCTLH-FILIAL TO CFLIAL-CGC-ST OF GFCTB0H9.           
ST2506     MOVE GFCTLH-CONTROLE TO CCTRL-CPF-CGC-ST OF GFCTB0H9.

           EXEC SQL
             SELECT
                   CCGC_CPF,
                   CCTRL_CPF_CGC,
                   CFLIAL_CGC
             INTO
                   :GFCTB0H9.CCGC-CPF,
                   :GFCTB0H9.CCTRL-CPF-CGC,
                   :GFCTB0H9.CFLIAL-CGC
             FROM DB2PRD.TADSAO_GRP_CLI
             WHERE CAGPTO_CTA = :GFCTB0H9.CAGPTO-CTA
              AND DINIC_ADSAO_COMP <= :GFCTB0H9.DINIC-ADSAO-COMP
ST2506*       AND CCGC_CPF = :GFCTB0H9.CCGC-CPF
ST2506*       AND CFLIAL_CGC = :GFCTB0H9.CFLIAL-CGC
ST2506*       AND CCTRL_CPF_CGC = :GFCTB0H9.CCTRL-CPF-CGC
ST2506        AND CCGC_CPF_ST = :GFCTB0H9.CCGC-CPF-ST
ST2506        AND CFLIAL_CGC_ST = :GFCTB0H9.CFLIAL-CGC-ST
ST2506        AND CCTRL_CPF_CGC_ST = :GFCTB0H9.CCTRL-CPF-CGC-ST
           END-EXEC.

BI0810     IF (SQLCODE NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0012' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'TADSAO_GRP_CLI' TO GFCT0M-NOME-TAB
              MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
              MOVE '0170' TO GFCT0M-LOCAL
              MOVE 0012 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB0H9' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

BI0810     IF SQLCODE EQUAL ZEROS OR -811
BI0810        PERFORM 2500-VERIFICA-DUPLICADO
           END-IF.

      *----------------------------------------------------------------*
       2332-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA EXISTENCIA DA EMPRESA NA TABELA GFCTB003           *
      ******************************************************************
      *----------------------------------------------------------------*
       2333-VERIFICAR-EMPRESA SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB001 TO CAGPTO-CTA OF GFCTB003.
           MOVE CSERVC-TARIF OF GFCTB001 TO CSERVC-TARIF OF GFCTB003.
           MOVE DPROCM-ATUAL OF GFCTB0A1 TO DINIC-ADSAO-COMP
              OF GFCTB003.
           MOVE GFCTLH-CAGENCIA TO CAG-EMPR OF GFCTB003.
           MOVE GFCTLH-CCONTA TO CNRO-CTA-DEB OF GFCTB003.

           EXEC SQL
             SELECT
                   CAG_EMPR,
                   CNRO_CTA_DEB
             INTO
                   :GFCTB003.CAG-EMPR,
                   :GFCTB003.CNRO-CTA-DEB
             FROM DB2PRD.ADSAO_GRP_EMPR
             WHERE CAGPTO_CTA = :GFCTB003.CAGPTO-CTA
              AND DINIC_ADSAO_COMP <= :GFCTB003.DINIC-ADSAO-COMP
              AND CAG_EMPR = :GFCTB003.CAG-EMPR
              AND CNRO_CTA_DEB = :GFCTB003.CNRO-CTA-DEB
           END-EXEC.

BI0810     IF (SQLCODE NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0012' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'ADSAO_GRP_EMPR' TO GFCT0M-NOME-TAB
              MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
              MOVE '0180' TO GFCT0M-LOCAL
              MOVE 0012 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB003' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

BI0810     IF SQLCODE EQUAL ZEROS OR -811
BI0810        PERFORM 2500-VERIFICA-DUPLICADO
           END-IF.

      *----------------------------------------------------------------*
       2333-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA EXISTENCIA DO POSTO NA TABELA GFCTB005             *
      ******************************************************************
      *----------------------------------------------------------------*
       2334-VERIFICAR-POSTO SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB001 TO CAGPTO-CTA OF GFCTB005.
           MOVE CSERVC-TARIF OF GFCTB001 TO CSERVC-TARIF OF GFCTB005.
           MOVE DPROCM-ATUAL OF GFCTB0A1 TO DINIC-ADSAO-COMP
              OF GFCTB005.
           MOVE 237 TO CEMPR-INC OF GFCTB005.
           MOVE GFCTLH-CAGENCIA TO CDEPDC OF GFCTB005.

           IF GFCTLH-TOTAL EQUAL 'S'
              MOVE ZEROS TO CPOSTO-SERVC OF GFCTB005
           ELSE
              MOVE GFCTLH-CPAB TO CPOSTO-SERVC OF GFCTB005
           END-IF.

           EXEC SQL
             SELECT
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
             INTO
                   :GFCTB005.CEMPR-INC,
                   :GFCTB005.CDEPDC,
                   :GFCTB005.CPOSTO-SERVC
             FROM DB2PRD.ADSAO_GRP_PAB
             WHERE CAGPTO_CTA = :GFCTB005.CAGPTO-CTA
              AND DINIC_ADSAO_COMP <= :GFCTB005.DINIC-ADSAO-COMP
              AND CEMPR_INC = :GFCTB005.CEMPR-INC
              AND CDEPDC = :GFCTB005.CDEPDC
              AND CPOSTO_SERVC = :GFCTB005.CPOSTO-SERVC
           END-EXEC.

BI0810     IF (SQLCODE NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0012' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'ADSAO_GRP_PAB' TO GFCT0M-NOME-TAB
              MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
              MOVE '0190' TO GFCT0M-LOCAL
              MOVE 0012 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB005' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

BI0810     IF SQLCODE EQUAL ZEROS OR -811
BI0810        PERFORM 2500-VERIFICA-DUPLICADO
              GO TO 2334-99-FIM
           END-IF.

           MOVE CAGPTO-CTA OF GFCTB001 TO CAGPTO-CTA OF GFCTB005.
           MOVE CSERVC-TARIF OF GFCTB001 TO CSERVC-TARIF OF GFCTB005.
           MOVE DPROCM-ATUAL OF GFCTB0A1 TO DINIC-ADSAO-COMP
              OF GFCTB005.
           MOVE 237 TO CEMPR-INC OF GFCTB005.
           MOVE GFCTLH-CAGENCIA TO CDEPDC OF GFCTB005.

           MOVE ZEROS TO CPOSTO-SERVC OF GFCTB005.

           IF GFCTLH-TOTAL EQUAL 'S'
              EXEC SQL
               SELECT
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
              INTO
                   :GFCTB005.CEMPR-INC,
                   :GFCTB005.CDEPDC,
                   :GFCTB005.CPOSTO-SERVC
              FROM DB2PRD.ADSAO_GRP_PAB
              WHERE CAGPTO_CTA = :GFCTB005.CAGPTO-CTA
              AND DINIC_ADSAO_COMP <= :GFCTB005.DINIC-ADSAO-COMP
              AND CEMPR_INC = :GFCTB005.CEMPR-INC
              AND CDEPDC = :GFCTB005.CDEPDC
              END-EXEC
              IF (SQLCODE NOT EQUAL ZEROS AND +100 AND -811) OR
                 (SQLWARN0 EQUAL 'W')
                 MOVE SPACES TO GFCT0M-AREA-ERROS
                 INITIALIZE GFCT0M-AREA-ERROS
                 MOVE '0012' TO GFCTG2-COD-MSG
                 PERFORM 1210-OBTER-DESC-MSG
                 MOVE 'ADSAO_GRP_PAB' TO GFCT0M-NOME-TAB
                 MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
                 MOVE '0200' TO GFCT0M-LOCAL
                 MOVE 0012 TO GFCTLI-COD-MSG-ERRO
                 MOVE ' - GFCTB005' TO WRK-NOME-TAB
                 PERFORM 1321-MOVER-ERRO-SQL
                 MOVE SPACES TO GFCTLI-DADOS-RST
                 PERFORM 3000-FINALIZAR
              END-IF
              IF (SQLCODE EQUAL ZEROS) OR
                 (SQLCODE EQUAL -811)
                 MOVE 1 TO GFCTLI-ERRO
                 MOVE 972 TO GFCTLI-COD-MSG-ERRO
                 MOVE '0972' TO GFCTG2-COD-MSG
                 MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
                 PERFORM 1210-OBTER-DESC-MSG
                 MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
                 PERFORM 3000-FINALIZAR
              END-IF
           ELSE
              EXEC SQL
               SELECT
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
              INTO
                   :GFCTB005.CEMPR-INC,
                   :GFCTB005.CDEPDC,
                   :GFCTB005.CPOSTO-SERVC
              FROM DB2PRD.ADSAO_GRP_PAB
              WHERE CAGPTO_CTA = :GFCTB005.CAGPTO-CTA
              AND DINIC_ADSAO_COMP <= :GFCTB005.DINIC-ADSAO-COMP
              AND CEMPR_INC = :GFCTB005.CEMPR-INC
              AND CDEPDC = :GFCTB005.CDEPDC
              AND CPOSTO_SERVC = :GFCTB005.CPOSTO-SERVC
            END-EXEC
BI0810        IF (SQLCODE NOT EQUAL ZEROS AND +100 AND -811) OR
                 (SQLWARN0 EQUAL 'W')
                 MOVE SPACES TO GFCT0M-AREA-ERROS
                 INITIALIZE GFCT0M-AREA-ERROS
                 MOVE '0012' TO GFCTG2-COD-MSG
                 PERFORM 1210-OBTER-DESC-MSG
                 MOVE 'ADSAO_GRP_PAB' TO GFCT0M-NOME-TAB
                 MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
                 MOVE '0210' TO GFCT0M-LOCAL
                 MOVE 0012 TO GFCTLI-COD-MSG-ERRO
                 MOVE ' - GFCTB005' TO WRK-NOME-TAB
                 PERFORM 1321-MOVER-ERRO-SQL
                 MOVE SPACES TO GFCTLI-DADOS-RST
                 PERFORM 3000-FINALIZAR
              END-IF
              IF (SQLCODE EQUAL ZEROS) OR
BI0810           (SQLCODE EQUAL -811)
                 MOVE 1 TO GFCTLI-ERRO
                 MOVE 973 TO GFCTLI-COD-MSG-ERRO
                 MOVE '0973' TO GFCTG2-COD-MSG
                 MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
                 PERFORM 1210-OBTER-DESC-MSG
                 MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
                 PERFORM 3000-FINALIZAR
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       2334-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA EXISTENCIA DO POSTAL NA TABELA GFCTB006            *
      ******************************************************************
      *----------------------------------------------------------------*
       2335-VERIFICAR-POSTAL-EXP SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB001 TO CAGPTO-CTA OF GFCTB006.
           MOVE CSERVC-TARIF OF GFCTB001 TO CSERVC-TARIF OF GFCTB006.
           MOVE DPROCM-ATUAL OF GFCTB0A1 TO DINIC-ADSAO-COMP
              OF GFCTB006.
           MOVE 237 TO CEMPR-INC OF GFCTB006.

           IF GFCTLH-TOTAL EQUAL 'S'
              MOVE ZEROS TO CDEPDC OF GFCTB006
              MOVE ZEROS TO CPOSTO-SERVC OF GFCTB006
           ELSE
              MOVE GFCTLH-CAGENCIA TO CDEPDC OF GFCTB006
              MOVE GFCTLH-CPAB TO CPOSTO-SERVC OF GFCTB006
           END-IF.

           EXEC SQL
             SELECT
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
             INTO
                   :GFCTB006.CEMPR-INC,
                   :GFCTB006.CDEPDC,
                   :GFCTB006.CPOSTO-SERVC
             FROM DB2PRD.ADSAO_GRP_PSTAL
             WHERE CAGPTO_CTA = :GFCTB006.CAGPTO-CTA
              AND DINIC_ADSAO_COMP <= :GFCTB006.DINIC-ADSAO-COMP
              AND CEMPR_INC = :GFCTB006.CEMPR-INC
              AND CDEPDC = :GFCTB006.CDEPDC
              AND CPOSTO_SERVC = :GFCTB006.CPOSTO-SERVC
           END-EXEC.

BI0810     IF (SQLCODE NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0012' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'ADSAO_GRP_PSTAL' TO GFCT0M-NOME-TAB
              MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
              MOVE '0220' TO GFCT0M-LOCAL
              MOVE 0012 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB006' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

BI0810     IF SQLCODE EQUAL ZEROS OR -811
BI0810        PERFORM 2500-VERIFICA-DUPLICADO
              GO TO 2335-99-FIM
           END-IF.

           MOVE CAGPTO-CTA OF GFCTB001 TO CAGPTO-CTA OF GFCTB006.
           MOVE CSERVC-TARIF OF GFCTB001 TO CSERVC-TARIF OF GFCTB006.
           MOVE DPROCM-ATUAL OF GFCTB0A1 TO DINIC-ADSAO-COMP
              OF GFCTB006.
           MOVE 237 TO CEMPR-INC OF GFCTB006.
           MOVE GFCTLH-CAGENCIA TO CDEPDC OF GFCTB006.
           MOVE ZEROS TO CPOSTO-SERVC OF GFCTB006.

           IF GFCTLH-TOTAL EQUAL 'S'
              EXEC SQL
               SELECT
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
               INTO
                   :GFCTB006.CEMPR-INC,
                   :GFCTB006.CDEPDC,
                   :GFCTB006.CPOSTO-SERVC
               FROM DB2PRD.ADSAO_GRP_PSTAL
               WHERE CAGPTO_CTA = :GFCTB006.CAGPTO-CTA
               AND DINIC_ADSAO_COMP <= :GFCTB006.DINIC-ADSAO-COMP
               AND CEMPR_INC = :GFCTB006.CEMPR-INC
               AND CDEPDC = :GFCTB006.CDEPDC
              END-EXEC
              IF (SQLCODE NOT EQUAL ZEROS AND +100 AND -811) OR
                 (SQLWARN0 EQUAL 'W')
                 MOVE SPACES TO GFCT0M-AREA-ERROS
                 INITIALIZE GFCT0M-AREA-ERROS
                 MOVE '0012' TO GFCTG2-COD-MSG
                 PERFORM 1210-OBTER-DESC-MSG
                 MOVE 'ADSAO_GRP_PSTAL' TO GFCT0M-NOME-TAB
                 MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
                 MOVE '0230' TO GFCT0M-LOCAL
                 MOVE 0012 TO GFCTLI-COD-MSG-ERRO
                 MOVE ' - GFCTB006' TO WRK-NOME-TAB
                 PERFORM 1321-MOVER-ERRO-SQL
                 MOVE SPACES TO GFCTLI-DADOS-RST
                 PERFORM 3000-FINALIZAR
              END-IF
              IF (SQLCODE EQUAL ZEROS) OR
                 (SQLCODE EQUAL -811)
                 MOVE 1 TO GFCTLI-ERRO
                 MOVE 976 TO GFCTLI-COD-MSG-ERRO
                 MOVE '0976' TO GFCTG2-COD-MSG
                 MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
                 PERFORM 1210-OBTER-DESC-MSG
                 MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
                 PERFORM 3000-FINALIZAR
              END-IF
           ELSE
              EXEC SQL
               SELECT
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
               INTO
                   :GFCTB006.CEMPR-INC,
                   :GFCTB006.CDEPDC,
                   :GFCTB006.CPOSTO-SERVC
               FROM DB2PRD.ADSAO_GRP_PSTAL
               WHERE CAGPTO_CTA = :GFCTB006.CAGPTO-CTA
               AND DINIC_ADSAO_COMP <= :GFCTB006.DINIC-ADSAO-COMP
               AND CEMPR_INC = :GFCTB006.CEMPR-INC
               AND CDEPDC = :GFCTB006.CDEPDC
               AND CPOSTO_SERVC = :GFCTB006.CPOSTO-SERVC
              END-EXEC
BI0810        IF (SQLCODE NOT EQUAL ZEROS AND +100 AND -811) OR
                 (SQLWARN0 EQUAL 'W')
                 MOVE SPACES TO GFCT0M-AREA-ERROS
                 INITIALIZE GFCT0M-AREA-ERROS
                 MOVE '0012' TO GFCTG2-COD-MSG
                 PERFORM 1210-OBTER-DESC-MSG
                 MOVE 'ADSAO_GRP_PSTAL' TO GFCT0M-NOME-TAB
                 MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
                 MOVE '0240' TO GFCT0M-LOCAL
                 MOVE 0012 TO GFCTLI-COD-MSG-ERRO
                 MOVE ' - GFCTB006' TO WRK-NOME-TAB
                 PERFORM 1321-MOVER-ERRO-SQL
                 MOVE SPACES TO GFCTLI-DADOS-RST
                 PERFORM 3000-FINALIZAR
              END-IF
              IF (SQLCODE EQUAL ZEROS) OR
BI0810           (SQLCODE EQUAL -811)
                 MOVE 1 TO GFCTLI-ERRO
                 MOVE 977 TO GFCTLI-COD-MSG-ERRO
                 MOVE '0977' TO GFCTG2-COD-MSG
                 MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
                 PERFORM 1210-OBTER-DESC-MSG
                 MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
                 PERFORM 3000-FINALIZAR
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       2335-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA EXISTENCIA DO SEGMENTO NA TABELA GFCTB007          *
      ******************************************************************
      *----------------------------------------------------------------*
       2336-VERIFICAR-SEGMENTO SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB001 TO CAGPTO-CTA OF GFCTB007.
           MOVE CSERVC-TARIF OF GFCTB001 TO CSERVC-TARIF OF GFCTB007.
           MOVE DPROCM-ATUAL OF GFCTB0A1 TO DINIC-ADSAO-COMP
              OF GFCTB007.
           MOVE GFCTLH-CSEGMENTO TO CSGMTO-GSTAO-TARIF
              OF GFCTB007.

           EXEC SQL
             SELECT
                   CSGMTO_GSTAO_TARIF
             INTO
                   :GFCTB007.CSGMTO-GSTAO-TARIF
             FROM DB2PRD.ADSAO_GRP_SGMTO
             WHERE CAGPTO_CTA = :GFCTB007.CAGPTO-CTA
              AND DINIC_ADSAO_COMP <= :GFCTB007.DINIC-ADSAO-COMP
              AND CSGMTO_GSTAO_TARIF = :GFCTB007.CSGMTO-GSTAO-TARIF
           END-EXEC.

BI0810     IF (SQLCODE NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0012' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'ADSAO_GRP_SGMTO' TO GFCT0M-NOME-TAB
              MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
              MOVE '0250' TO GFCT0M-LOCAL
              MOVE 0012 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB007' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

BI0810     IF SQLCODE EQUAL ZEROS OR -811
BI0810        PERFORM 2500-VERIFICA-DUPLICADO
           END-IF.

      *----------------------------------------------------------------*
       2336-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA EXISTENCIA DO MUNICIPIO NA TABELA GFCTB004         *
      ******************************************************************
      *----------------------------------------------------------------*
       2337-VERIFICAR-MUNICIPIO SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB001 TO CAGPTO-CTA OF GFCTB004.
           MOVE CSERVC-TARIF OF GFCTB001 TO CSERVC-TARIF OF GFCTB004.
           MOVE DPROCM-ATUAL OF GFCTB0A1 TO DINIC-ADSAO-COMP
              OF GFCTB004.
           MOVE GFCTLH-CMUNIC TO CMUN-IBGE OF GFCTB004.

           EXEC SQL
             SELECT
                   CMUN_IBGE
             INTO
                   :GFCTB004.CMUN-IBGE
             FROM DB2PRD.ADSAO_GRP_MUN
             WHERE CAGPTO_CTA = :GFCTB004.CAGPTO-CTA
              AND DINIC_ADSAO_COMP <= :GFCTB004.DINIC-ADSAO-COMP
              AND CMUN_IBGE = :GFCTB004.CMUN-IBGE
           END-EXEC.

BI0810     IF (SQLCODE NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0012' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'ADSAO_GRP_MUN' TO GFCT0M-NOME-TAB
              MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
              MOVE '0260' TO GFCT0M-LOCAL
              MOVE 0012 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB007' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

BI0810     IF SQLCODE EQUAL ZEROS OR -811
BI0810        PERFORM 2500-VERIFICA-DUPLICADO
           END-IF.

      *----------------------------------------------------------------*
       2337-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA EXISTENCIA DA UF NA TABELA GFCTB008                *
      ******************************************************************
      *----------------------------------------------------------------*
       2338-VERIFICAR-UF SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB001 TO CAGPTO-CTA OF GFCTB008.
           MOVE CSERVC-TARIF OF GFCTB001 TO CSERVC-TARIF OF GFCTB008.
           MOVE DPROCM-ATUAL OF GFCTB0A1 TO DINIC-ADSAO-COMP
              OF GFCTB008.
           MOVE GFCTLH-CUF TO CSGL-UF OF GFCTB008.

           EXEC SQL
             SELECT
                   CSGL_UF
             INTO
                   :GFCTB008.CSGL-UF
             FROM DB2PRD.ADSAO_GRP_UF
             WHERE CAGPTO_CTA = :GFCTB008.CAGPTO-CTA
              AND DINIC_ADSAO_COMP <= :GFCTB008.DINIC-ADSAO-COMP
              AND CSGL_UF = :GFCTB008.CSGL-UF
           END-EXEC.

BI0810     IF (SQLCODE NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0012' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'ADSAO_GRP_UF' TO GFCT0M-NOME-TAB
              MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
              MOVE '0270' TO GFCT0M-LOCAL
              MOVE 0012 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB007' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

BI0810     IF SQLCODE EQUAL ZEROS OR -811
BI0810        PERFORM 2500-VERIFICA-DUPLICADO
           END-IF.

      *----------------------------------------------------------------*
       2338-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FECHAMENTO DO CURSOR DA TABELA GFCTB001                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2340-CLOSE-CSR01-GFCTB001 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                   CLOSE CSR01-GFCTB001
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0011' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'ADSAO_COMP_PCOTE' TO GFCT0M-NOME-TAB
              MOVE 'CLOSE' TO GFCT0M-COMANDO-SQL
              MOVE '0280' TO GFCT0M-LOCAL
              MOVE 0011 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB001' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2340-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    INSERIR REGISTROS - ADESAO COMPULSORIA                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-INSERIR SECTION.
      *----------------------------------------------------------------*

           PERFORM 2400-10-VER-FUTURO.

           PERFORM 2410-INSERIR-DADOS-GFCTB001.

           EVALUATE GFCTLH-CAGPTO
           WHEN 15
                PERFORM 2420-INSERIR-PAB
           WHEN 14
                PERFORM 2430-INSERIR-AGENCIA
           WHEN 16
                PERFORM 2440-INSERIR-EMPRESA
           WHEN 17
                PERFORM 2450-INSERIR-POSTAL-EXP
           WHEN 20
                PERFORM 2450-INSERIR-POSTAL-EXP
           WHEN 4
                PERFORM 2460-INSERIR-CLIENTE
           WHEN 9
                PERFORM 2470-INSERIR-SEGMENTO
           WHEN 18
                PERFORM 2480-INSERIR-MUNICIPIO
           WHEN 19
                PERFORM 2490-INSERIR-UF
           END-EVALUATE.

      *----------------------------------------------------------------*
       2400-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2400-10-VER-FUTURO SECTION.
      *----------------------------------------------------------------*

           PERFORM 2400-11-ABRIR-CURSOR.

           PERFORM 2400-12-LER-CURSOR.

           PERFORM 2400-13-PROCESSAR-CURSOR2 UNTIL
              WRK-FIM EQUAL 'S' OR
              WRK-ACHOU-FUTURO EQUAL 'S'.

           PERFORM 2400-14-FECHAR-CURSOR.

           IF WRK-ACHOU-FUTURO EQUAL 'S'
              MOVE 1 TO GFCTLI-ERRO
              MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
              MOVE 0966 TO GFCTLI-COD-MSG-ERRO
              MOVE '0966' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2400-10-99-FIM.                 
           EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2400-11-ABRIR-CURSOR SECTION.
      *----------------------------------------------------------------*

           MOVE 'N' TO WRK-FIM.
           MOVE 'N' TO WRK-ACHOU-FUTURO.
           MOVE GFCTLH-CPACOTE TO CSERVC-TARIF OF GFCTB001.
           MOVE GFCTLH-CAGPTO TO CAGPTO-CTA OF GFCTB001.
           MOVE DPROCM-ATUAL OF GFCTB0A1
              TO DINIC-ADSAO-COMP OF GFCTB001.

           EXEC SQL
               OPEN CSR02-GFCTB001
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0011' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'ADSAO_COMP_PCOTE' TO GFCT0M-NOME-TAB
              MOVE 'OPEN ' TO GFCT0M-COMANDO-SQL
              MOVE '0290' TO GFCT0M-LOCAL
              MOVE 0011 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB001' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2400-11-99-FIM.                 
           EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2400-12-LER-CURSOR SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR02-GFCTB001
               INTO :GFCTB001.CSERVC-TARIF,
                    :GFCTB001.CAGPTO-CTA,
                    :GFCTB001.DINIC-ADSAO-COMP,
                    :GFCTB001.CSEQ-AGPTO-CTA,
BI0810               :GFCTB001.CPSSOA-SERVC-TARIF
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0011' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'ADSAO_COMP_PCOTE' TO GFCT0M-NOME-TAB
              MOVE 'FETCH' TO GFCT0M-COMANDO-SQL
              MOVE '0300' TO GFCT0M-LOCAL
              MOVE 0011 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB001' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

           IF SQLCODE EQUAL +100
              MOVE 'S' TO WRK-FIM
           END-IF.

      *----------------------------------------------------------------*
       2400-12-99-FIM.                 
           EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2400-13-PROCESSAR-CURSOR2 SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTLH-CAGPTO
           WHEN 14
                PERFORM 2400-1-VERIFICAR-AGENCIA
           WHEN 4
                PERFORM 2400-2-VERIFICAR-CLIENTE
           WHEN 16
                PERFORM 2400-3-VERIFICAR-EMPRESA
           WHEN 15
                PERFORM 2400-4-VERIFICAR-POSTO
           WHEN 17
                PERFORM 2400-5-VERIFICAR-POSTAL-EXP
           WHEN 20
                PERFORM 2400-5-VERIFICAR-POSTAL-EXP
           WHEN 9
                PERFORM 2400-6-VERIFICAR-SEGMENTO
           WHEN 18
                PERFORM 2400-7-VERIFICAR-MUNICIPIO
           WHEN 19
                PERFORM 2400-8-VERIFICAR-UF
           END-EVALUATE.

           PERFORM 2400-12-LER-CURSOR.

      *----------------------------------------------------------------*
       2400-13-99-FIM.                 
           EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2400-1-VERIFICAR-AGENCIA SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB001
              TO CAGPTO-CTA OF GFCTB002.
           MOVE CSERVC-TARIF OF GFCTB001
              TO CSERVC-TARIF OF GFCTB002.
           MOVE DINIC-ADSAO-COMP OF GFCTB001
              TO DINIC-ADSAO-COMP OF GFCTB002.
           MOVE 237 TO CEMPR-INC OF GFCTB002.

           IF GFCTLH-TOTAL EQUAL 'S'
              MOVE ZEROS TO CDEPDC OF GFCTB002
           ELSE
              MOVE GFCTLH-CAGENCIA TO CDEPDC OF GFCTB002
           END-IF.

           EXEC SQL
               SELECT CEMPR_INC,
                       CDEPDC
               INTO :GFCTB002.CEMPR-INC,
                      :GFCTB002.CDEPDC
               FROM DB2PRD.ADSAO_GRP_DEPDC
               WHERE CAGPTO_CTA = :GFCTB002.CAGPTO-CTA
                 AND DINIC_ADSAO_COMP = :GFCTB002.DINIC-ADSAO-COMP
                 AND CEMPR_INC = :GFCTB002.CEMPR-INC
                 AND CDEPDC = :GFCTB002.CDEPDC
           END-EXEC.

BI0810     IF (SQLCODE NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0011' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'ADSAO_GRP_DEPDC ' TO GFCT0M-NOME-TAB
              MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
              MOVE '0310' TO GFCT0M-LOCAL
              MOVE 0011 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB002' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

BI0810     IF SQLCODE EQUAL ZEROS OR -811
BI0810        PERFORM 2510-VERIFICA-FUTURO
              GO TO 2400-1-99-FIM
           END-IF.

*********** A PARTIR DAQUI EH PARA BRECAR AGENCIA TOTAL

           MOVE CAGPTO-CTA OF GFCTB001
              TO CAGPTO-CTA OF GFCTB002
           MOVE CSERVC-TARIF OF GFCTB001
              TO CSERVC-TARIF OF GFCTB002
           MOVE DINIC-ADSAO-COMP OF GFCTB001
              TO DINIC-ADSAO-COMP OF GFCTB002
           MOVE 237 TO CEMPR-INC OF GFCTB002

           MOVE ZEROS TO CDEPDC OF GFCTB002

           IF GFCTLH-TOTAL EQUAL 'S'
              EXEC SQL
               SELECT CEMPR_INC,
                       CDEPDC
               INTO :GFCTB002.CEMPR-INC,
                      :GFCTB002.CDEPDC
               FROM DB2PRD.ADSAO_GRP_DEPDC
               WHERE CAGPTO_CTA = :GFCTB002.CAGPTO-CTA
                 AND DINIC_ADSAO_COMP = :GFCTB002.DINIC-ADSAO-COMP
              END-EXEC
              IF (SQLCODE NOT EQUAL ZEROS AND +100 AND -811) OR
                 (SQLWARN0 EQUAL 'W')
                 MOVE SPACES TO GFCT0M-AREA-ERROS
                 INITIALIZE GFCT0M-AREA-ERROS
                 MOVE '0011' TO GFCTG2-COD-MSG
                 PERFORM 1210-OBTER-DESC-MSG
                 MOVE 'ADSAO_GRP_DEPDC ' TO GFCT0M-NOME-TAB
                 MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
                 MOVE '0320' TO GFCT0M-LOCAL
                 MOVE 0011 TO GFCTLI-COD-MSG-ERRO
                 MOVE ' - GFCTB002' TO WRK-NOME-TAB
                 PERFORM 1321-MOVER-ERRO-SQL
                 MOVE SPACES TO GFCTLI-DADOS-RST
                 PERFORM 3000-FINALIZAR
              END-IF
              IF (SQLCODE EQUAL ZEROS) OR (SQLCODE EQUAL -811)
                 MOVE 1 TO GFCTLI-ERRO
                 MOVE 970 TO GFCTLI-COD-MSG-ERRO
                 MOVE '0970' TO GFCTG2-COD-MSG
                 MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
                 PERFORM 1210-OBTER-DESC-MSG
                 MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
                 PERFORM 3000-FINALIZAR
              END-IF
           ELSE
              EXEC SQL
               SELECT CEMPR_INC,
                       CDEPDC
               INTO :GFCTB002.CEMPR-INC,
                      :GFCTB002.CDEPDC
               FROM DB2PRD.ADSAO_GRP_DEPDC
               WHERE CAGPTO_CTA = :GFCTB002.CAGPTO-CTA
                 AND DINIC_ADSAO_COMP = :GFCTB002.DINIC-ADSAO-COMP
                 AND CEMPR_INC = :GFCTB002.CEMPR-INC
                 AND CDEPDC = :GFCTB002.CDEPDC
              END-EXEC
BI0810        IF (SQLCODE NOT EQUAL ZEROS AND +100 AND -811) OR
                 (SQLWARN0 EQUAL 'W')
                 MOVE SPACES TO GFCT0M-AREA-ERROS
                 INITIALIZE GFCT0M-AREA-ERROS
                 MOVE '0011' TO GFCTG2-COD-MSG
                 PERFORM 1210-OBTER-DESC-MSG
                 MOVE 'ADSAO_GRP_DEPDC ' TO GFCT0M-NOME-TAB
                 MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
                 MOVE '0330' TO GFCT0M-LOCAL
                 MOVE 0011 TO GFCTLI-COD-MSG-ERRO
                 MOVE ' - GFCTB002' TO WRK-NOME-TAB
                 PERFORM 1321-MOVER-ERRO-SQL
                 MOVE SPACES TO GFCTLI-DADOS-RST
                 PERFORM 3000-FINALIZAR
              END-IF
BI0810        IF SQLCODE EQUAL ZEROS OR -811
                 MOVE 1 TO GFCTLI-ERRO
                 MOVE 971 TO GFCTLI-COD-MSG-ERRO
                 MOVE '0971' TO GFCTG2-COD-MSG
                 MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
                 PERFORM 1210-OBTER-DESC-MSG
                 MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
                 PERFORM 3000-FINALIZAR
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       2400-1-99-FIM.                  
           EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2400-2-VERIFICAR-CLIENTE SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB001
              TO CAGPTO-CTA OF GFCTB0H9.
           MOVE CSERVC-TARIF OF GFCTB001
              TO CSERVC-TARIF OF GFCTB0H9.
           MOVE DINIC-ADSAO-COMP OF GFCTB001
              TO DINIC-ADSAO-COMP OF GFCTB0H9.

ST2506*    M0VE GFCTLH-CGC-CPF TO CCGC-CPF OF GFCTB0H9.
ST2506*    M0VE GFCTLH-FILIAL TO CFLIAL-CGC OF GFCTB0H9.
ST2506*    M0VE GFCTLH-CONTROLE TO CCTRL-CPF-CGC OF GFCTB0H9.
ST2506     MOVE GFCTLH-CGC-CPF TO CCGC-CPF-ST OF GFCTB0H9.
ST2506     MOVE GFCTLH-FILIAL TO CFLIAL-CGC-ST OF GFCTB0H9.
ST2506     MOVE GFCTLH-CONTROLE TO CCTRL-CPF-CGC-ST OF GFCTB0H9.

           EXEC SQL
               SELECT CCGC_CPF,
                       CFLIAL_CGC,
                       CCTRL_CPF_CGC
               INTO :GFCTB0H9.CCGC-CPF,
                      :GFCTB0H9.CFLIAL-CGC,
                      :GFCTB0H9.CCTRL-CPF-CGC
               FROM DB2PRD.TADSAO_GRP_CLI
               WHERE CAGPTO_CTA = :GFCTB0H9.CAGPTO-CTA
                 AND DINIC_ADSAO_COMP = :GFCTB0H9.DINIC-ADSAO-COMP
ST2506*          AND CCGC_CPF = :GFCTB0H9.CCGC-CPF
ST2506*          AND CFLIAL_CGC = :GFCTB0H9.CFLIAL-CGC
ST2506*          AND CCTRL_CPF_CGC = :GFCTB0H9.CCTRL-CPF-CGC
ST2506           AND CCGC_CPF_ST = :GFCTB0H9.CCGC-CPF-ST
ST2506           AND CFLIAL_CGC_ST = :GFCTB0H9.CFLIAL-CGC-ST
                 AND CCTRL_CPF_CGC_ST = :GFCTB0H9.CCTRL-CPF-CGC-ST
           END-EXEC.

BI0810     IF (SQLCODE NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0011' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'TADSAO_GRP_CLI  ' TO GFCT0M-NOME-TAB
              MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
              MOVE '0340' TO GFCT0M-LOCAL
              MOVE 0011 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB0H9' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

BI0810     IF SQLCODE EQUAL ZEROS OR -811
BI0810        PERFORM 2510-VERIFICA-FUTURO
           END-IF.

      *----------------------------------------------------------------*
       2400-2-99-FIM.                  
           EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2400-3-VERIFICAR-EMPRESA SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB001
              TO CAGPTO-CTA OF GFCTB003.
           MOVE CSERVC-TARIF OF GFCTB001
              TO CSERVC-TARIF OF GFCTB003.
           MOVE DINIC-ADSAO-COMP OF GFCTB001
              TO DINIC-ADSAO-COMP OF GFCTB003.

           MOVE GFCTLH-CAGENCIA TO CAG-EMPR OF GFCTB003.
           MOVE GFCTLH-CCONTA TO CNRO-CTA-DEB OF GFCTB003.

           EXEC SQL
               SELECT CAG_EMPR,
                       CNRO_CTA_DEB
               INTO :GFCTB003.CAG-EMPR,
                      :GFCTB003.CNRO-CTA-DEB
               FROM DB2PRD.ADSAO_GRP_EMPR
               WHERE CAGPTO_CTA = :GFCTB003.CAGPTO-CTA
                 AND DINIC_ADSAO_COMP = :GFCTB003.DINIC-ADSAO-COMP
                 AND CAG_EMPR = :GFCTB003.CAG-EMPR
                 AND CNRO_CTA_DEB = :GFCTB003.CNRO-CTA-DEB
           END-EXEC.

BI0810     IF (SQLCODE NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0011' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'TADSAO_GRP_EMP  ' TO GFCT0M-NOME-TAB
              MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
              MOVE '0350' TO GFCT0M-LOCAL
              MOVE 0011 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB003' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

BI0810     IF SQLCODE EQUAL ZEROS OR -811
BI0810        PERFORM 2510-VERIFICA-FUTURO
           END-IF.

      *----------------------------------------------------------------*
       2400-3-99-FIM.                  
           EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2400-4-VERIFICAR-POSTO SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB001
              TO CAGPTO-CTA OF GFCTB005.
           MOVE CSERVC-TARIF OF GFCTB001
              TO CSERVC-TARIF OF GFCTB005.
           MOVE DINIC-ADSAO-COMP OF GFCTB001
              TO DINIC-ADSAO-COMP OF GFCTB005.

           MOVE 237 TO CEMPR-INC OF GFCTB005.

           MOVE GFCTLH-CAGENCIA TO CDEPDC OF GFCTB005.

           IF GFCTLH-TOTAL EQUAL 'S'
              MOVE ZEROS TO CPOSTO-SERVC OF GFCTB005
           ELSE
              MOVE GFCTLH-CPAB TO CPOSTO-SERVC OF GFCTB005
           END-IF.

           EXEC SQL
               SELECT CEMPR_INC,
                       CDEPDC,
                       CPOSTO_SERVC
               INTO :GFCTB005.CEMPR-INC,
                      :GFCTB005.CDEPDC,
                      :GFCTB005.CPOSTO-SERVC
               FROM DB2PRD.ADSAO_GRP_PAB
               WHERE CAGPTO_CTA = :GFCTB005.CAGPTO-CTA
                 AND DINIC_ADSAO_COMP = :GFCTB005.DINIC-ADSAO-COMP
                 AND CEMPR_INC = :GFCTB005.CEMPR-INC
                 AND CDEPDC = :GFCTB005.CDEPDC
                 AND CPOSTO_SERVC = :GFCTB005.CPOSTO-SERVC
           END-EXEC.

BI0810     IF (SQLCODE NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0011' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'TADSAO_GRP_PAB  ' TO GFCT0M-NOME-TAB
              MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
              MOVE '0360' TO GFCT0M-LOCAL
              MOVE 0011 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB005' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

BI0810     IF SQLCODE EQUAL ZEROS OR -811
BI0810        PERFORM 2510-VERIFICA-FUTURO
              GO TO 2400-4-99-FIM
           END-IF.

************ AQUI COMECA A VERIFICACAO DE PAB ZERADO

           MOVE CAGPTO-CTA OF GFCTB001
              TO CAGPTO-CTA OF GFCTB005.
           MOVE CSERVC-TARIF OF GFCTB001
              TO CSERVC-TARIF OF GFCTB005.
           MOVE DINIC-ADSAO-COMP OF GFCTB001
              TO DINIC-ADSAO-COMP OF GFCTB005.

           MOVE 237 TO CEMPR-INC OF GFCTB005.

           MOVE GFCTLH-CAGENCIA TO CDEPDC OF GFCTB005.

           MOVE ZEROS TO CPOSTO-SERVC OF GFCTB005.

           IF GFCTLH-TOTAL EQUAL 'S'
              EXEC SQL
               SELECT CEMPR_INC,
                       CDEPDC,
                       CPOSTO_SERVC
               INTO :GFCTB005.CEMPR-INC,
                      :GFCTB005.CDEPDC,
                      :GFCTB005.CPOSTO-SERVC
               FROM DB2PRD.ADSAO_GRP_PAB
               WHERE CAGPTO_CTA = :GFCTB005.CAGPTO-CTA
                 AND DINIC_ADSAO_COMP = :GFCTB005.DINIC-ADSAO-COMP
                 AND CEMPR_INC = :GFCTB005.CEMPR-INC
                 AND CDEPDC = :GFCTB005.CDEPDC
              END-EXEC
              IF (SQLCODE NOT EQUAL ZEROS AND +100 AND -811) OR
                 (SQLWARN0 EQUAL 'W')
                 MOVE SPACES TO GFCT0M-AREA-ERROS
                 INITIALIZE GFCT0M-AREA-ERROS
                 MOVE '0011' TO GFCTG2-COD-MSG
                 PERFORM 1210-OBTER-DESC-MSG
                 MOVE 'TADSAO_GRP_PAB  ' TO GFCT0M-NOME-TAB
                 MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
                 MOVE '0370' TO GFCT0M-LOCAL
                 MOVE 0011 TO GFCTLI-COD-MSG-ERRO
                 MOVE ' - GFCTB005' TO WRK-NOME-TAB
                 PERFORM 1321-MOVER-ERRO-SQL
                 MOVE SPACES TO GFCTLI-DADOS-RST
                 PERFORM 3000-FINALIZAR
              END-IF
              IF (SQLCODE EQUAL ZEROS) OR
                 (SQLCODE EQUAL -811)
                 MOVE 1 TO GFCTLI-ERRO
                 MOVE 974 TO GFCTLI-COD-MSG-ERRO
                 MOVE '0974' TO GFCTG2-COD-MSG
                 MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
                 PERFORM 1210-OBTER-DESC-MSG
                 MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
                 PERFORM 3000-FINALIZAR
              END-IF
           ELSE
              EXEC SQL
               SELECT CEMPR_INC,
                       CDEPDC,
                       CPOSTO_SERVC
               INTO :GFCTB005.CEMPR-INC,
                      :GFCTB005.CDEPDC,
                      :GFCTB005.CPOSTO-SERVC
               FROM DB2PRD.ADSAO_GRP_PAB
               WHERE CAGPTO_CTA = :GFCTB005.CAGPTO-CTA
                 AND DINIC_ADSAO_COMP = :GFCTB005.DINIC-ADSAO-COMP
                 AND CEMPR_INC = :GFCTB005.CEMPR-INC
                 AND CDEPDC = :GFCTB005.CDEPDC
                 AND CPOSTO_SERVC = :GFCTB005.CPOSTO-SERVC
              END-EXEC
BI0810        IF (SQLCODE NOT EQUAL ZEROS AND +100 AND -811) OR
                 (SQLWARN0 EQUAL 'W')
                 MOVE SPACES TO GFCT0M-AREA-ERROS
                 INITIALIZE GFCT0M-AREA-ERROS
                 MOVE '0011' TO GFCTG2-COD-MSG
                 PERFORM 1210-OBTER-DESC-MSG
                 MOVE 'TADSAO_GRP_PAB  ' TO GFCT0M-NOME-TAB
                 MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
                 MOVE '0380' TO GFCT0M-LOCAL
                 MOVE 0011 TO GFCTLI-COD-MSG-ERRO
                 MOVE ' - GFCTB005' TO WRK-NOME-TAB
                 PERFORM 1321-MOVER-ERRO-SQL
                 MOVE SPACES TO GFCTLI-DADOS-RST
                 PERFORM 3000-FINALIZAR
              END-IF
BI0810        IF SQLCODE EQUAL ZEROS OR -811
                 MOVE 1 TO GFCTLI-ERRO
                 MOVE 975 TO GFCTLI-COD-MSG-ERRO
                 MOVE '0975' TO GFCTG2-COD-MSG
                 MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
                 PERFORM 1210-OBTER-DESC-MSG
                 MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
                 PERFORM 3000-FINALIZAR
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       2400-4-99-FIM.                  
           EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2400-5-VERIFICAR-POSTAL-EXP SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB001
              TO CAGPTO-CTA OF GFCTB006.
           MOVE CSERVC-TARIF OF GFCTB001
              TO CSERVC-TARIF OF GFCTB006.
           MOVE DINIC-ADSAO-COMP OF GFCTB001
              TO DINIC-ADSAO-COMP OF GFCTB006.

           MOVE 237 TO CEMPR-INC OF GFCTB006.

           IF GFCTLH-TOTAL EQUAL 'S'
              MOVE ZEROS TO CDEPDC OF GFCTB006
              MOVE ZEROS TO CPOSTO-SERVC OF GFCTB006
           ELSE
              MOVE GFCTLH-CAGENCIA TO CDEPDC OF GFCTB006
              MOVE GFCTLH-CPAB TO CPOSTO-SERVC OF GFCTB006
           END-IF.

           EXEC SQL
               SELECT CEMPR_INC,
                       CDEPDC,
                       CPOSTO_SERVC
               INTO :GFCTB006.CEMPR-INC,
                      :GFCTB006.CDEPDC,
                      :GFCTB006.CPOSTO-SERVC
               FROM DB2PRD.ADSAO_GRP_PSTAL
               WHERE CAGPTO_CTA = :GFCTB006.CAGPTO-CTA
                 AND DINIC_ADSAO_COMP = :GFCTB006.DINIC-ADSAO-COMP
                 AND CEMPR_INC = :GFCTB006.CEMPR-INC
                 AND CDEPDC = :GFCTB006.CDEPDC
                 AND CPOSTO_SERVC = :GFCTB006.CPOSTO-SERVC
           END-EXEC.

BI0810     IF (SQLCODE NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0011' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'TADSAO_GRP_PSTAL' TO GFCT0M-NOME-TAB
              MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
              MOVE '0390' TO GFCT0M-LOCAL
              MOVE 0011 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB006' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

BI0810     IF SQLCODE EQUAL ZEROS OR -811
BI0810        PERFORM 2510-VERIFICA-FUTURO
              GO TO 2400-5-99-FIM
           END-IF.

************* COMECA VERIFICACAO DE PAB ZERO.

           MOVE CAGPTO-CTA OF GFCTB001
              TO CAGPTO-CTA OF GFCTB006.
           MOVE CSERVC-TARIF OF GFCTB001
              TO CSERVC-TARIF OF GFCTB006.
           MOVE DINIC-ADSAO-COMP OF GFCTB001
              TO DINIC-ADSAO-COMP OF GFCTB006.

           MOVE 237 TO CEMPR-INC OF GFCTB006.

           MOVE GFCTLH-CAGENCIA TO CDEPDC OF GFCTB006.

           MOVE ZEROS TO CPOSTO-SERVC OF GFCTB006.

           IF GFCTLH-TOTAL EQUAL 'S'
               EXEC SQL
                SELECT CEMPR_INC,
                       CDEPDC,
                       CPOSTO_SERVC
                INTO :GFCTB006.CEMPR-INC,
                      :GFCTB006.CDEPDC,
                      :GFCTB006.CPOSTO-SERVC
                FROM DB2PRD.ADSAO_GRP_PSTAL
                WHERE CAGPTO_CTA = :GFCTB006.CAGPTO-CTA
                 AND DINIC_ADSAO_COMP = :GFCTB006.DINIC-ADSAO-COMP
                 AND CEMPR_INC = :GFCTB006.CEMPR-INC
                 AND CDEPDC = :GFCTB006.CDEPDC
               END-EXEC
              IF (SQLCODE NOT EQUAL ZEROS AND +100 AND -811) OR
                 (SQLWARN0 EQUAL 'W')
                 MOVE SPACES TO GFCT0M-AREA-ERROS
                 INITIALIZE GFCT0M-AREA-ERROS
                 MOVE '0011' TO GFCTG2-COD-MSG
                 PERFORM 1210-OBTER-DESC-MSG
                 MOVE 'TADSAO_GRP_PSTAL' TO GFCT0M-NOME-TAB
                 MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
                 MOVE '0400' TO GFCT0M-LOCAL
                 MOVE 0011 TO GFCTLI-COD-MSG-ERRO
                 MOVE ' - GFCTB006' TO WRK-NOME-TAB
                 PERFORM 1321-MOVER-ERRO-SQL
                 MOVE SPACES TO GFCTLI-DADOS-RST
                 PERFORM 3000-FINALIZAR
              END-IF
              IF (SQLCODE EQUAL ZEROS) OR
                 (SQLCODE EQUAL -811)
                 MOVE 1 TO GFCTLI-ERRO
                 MOVE 978 TO GFCTLI-COD-MSG-ERRO
                 MOVE '0978' TO GFCTG2-COD-MSG
                 MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
                 PERFORM 1210-OBTER-DESC-MSG
                 MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
                 PERFORM 3000-FINALIZAR
              END-IF
           ELSE
               EXEC SQL
                SELECT CEMPR_INC,
                       CDEPDC,
                       CPOSTO_SERVC
                INTO :GFCTB006.CEMPR-INC,
                      :GFCTB006.CDEPDC,
                      :GFCTB006.CPOSTO-SERVC
                FROM DB2PRD.ADSAO_GRP_PSTAL
                WHERE CAGPTO_CTA = :GFCTB006.CAGPTO-CTA
                 AND DINIC_ADSAO_COMP = :GFCTB006.DINIC-ADSAO-COMP
                 AND CEMPR_INC = :GFCTB006.CEMPR-INC
                 AND CDEPDC = :GFCTB006.CDEPDC
                 AND CPOSTO_SERVC = :GFCTB006.CPOSTO-SERVC
               END-EXEC
BI0810        IF (SQLCODE NOT EQUAL ZEROS AND +100 AND -811) OR
                 (SQLWARN0 EQUAL 'W')
                 MOVE SPACES TO GFCT0M-AREA-ERROS
                 INITIALIZE GFCT0M-AREA-ERROS
                 MOVE '0011' TO GFCTG2-COD-MSG
                 PERFORM 1210-OBTER-DESC-MSG
                 MOVE 'TADSAO_GRP_PSTAL' TO GFCT0M-NOME-TAB
                 MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
                 MOVE '0410' TO GFCT0M-LOCAL
                 MOVE 0011 TO GFCTLI-COD-MSG-ERRO
                 MOVE ' - GFCTB006' TO WRK-NOME-TAB
                 PERFORM 1321-MOVER-ERRO-SQL
                 MOVE SPACES TO GFCTLI-DADOS-RST
                 PERFORM 3000-FINALIZAR
              END-IF
BI0810        IF SQLCODE EQUAL ZEROS OR -811
                 MOVE 1 TO GFCTLI-ERRO
                 MOVE 979 TO GFCTLI-COD-MSG-ERRO
                 MOVE '0979' TO GFCTG2-COD-MSG
                 MOVE ZEROS TO GFCTLI-COD-SQL-ERRO
                 PERFORM 1210-OBTER-DESC-MSG
                 MOVE GFCTG3-DESC-MSG TO GFCTLI-DESC-MSG-ERRO
                 PERFORM 3000-FINALIZAR
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       2400-5-99-FIM.                  
           EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2400-6-VERIFICAR-SEGMENTO SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB001
              TO CAGPTO-CTA OF GFCTB007.
           MOVE CSERVC-TARIF OF GFCTB001
              TO CSERVC-TARIF OF GFCTB007.
           MOVE DINIC-ADSAO-COMP OF GFCTB001
              TO DINIC-ADSAO-COMP OF GFCTB007.

           MOVE GFCTLH-CSEGMENTO TO CSGMTO-GSTAO-TARIF
              OF GFCTB007.

           EXEC SQL
               SELECT CSGMTO_GSTAO_TARIF
               INTO :GFCTB007.CSGMTO-GSTAO-TARIF
               FROM DB2PRD.ADSAO_GRP_SGMTO
               WHERE CAGPTO_CTA = :GFCTB007.CAGPTO-CTA
                 AND DINIC_ADSAO_COMP = :GFCTB007.DINIC-ADSAO-COMP
                 AND CSGMTO_GSTAO_TARIF = :GFCTB007.CSGMTO-GSTAO-TARIF
           END-EXEC.

BI0810     IF (SQLCODE NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0011' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'TADSAO_GRP_SEGTO' TO GFCT0M-NOME-TAB
              MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
              MOVE '0420' TO GFCT0M-LOCAL
              MOVE 0011 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB007' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

           IF SQLCODE EQUAL ZEROS OR -811
BI0810        PERFORM 2510-VERIFICA-FUTURO
           END-IF.

      *----------------------------------------------------------------*
       2400-6-99-FIM.                  
           EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2400-7-VERIFICAR-MUNICIPIO SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB001
              TO CAGPTO-CTA OF GFCTB004.
           MOVE CSERVC-TARIF OF GFCTB001
              TO CSERVC-TARIF OF GFCTB004.
           MOVE DINIC-ADSAO-COMP OF GFCTB001
              TO DINIC-ADSAO-COMP OF GFCTB004.

           MOVE GFCTLH-CMUNIC TO CMUN-IBGE OF GFCTB004.

           EXEC SQL
               SELECT CMUN_IBGE
               INTO :GFCTB004.CMUN-IBGE
               FROM DB2PRD.ADSAO_GRP_MUN
               WHERE CAGPTO_CTA = :GFCTB004.CAGPTO-CTA
                 AND DINIC_ADSAO_COMP = :GFCTB004.DINIC-ADSAO-COMP
                 AND CMUN_IBGE = :GFCTB004.CMUN-IBGE
           END-EXEC.

BI0810     IF (SQLCODE NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0011' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'TADSAO_GRP_MUN  ' TO GFCT0M-NOME-TAB
              MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
              MOVE '0430' TO GFCT0M-LOCAL
              MOVE 0011 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB007' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

BI0810     IF SQLCODE EQUAL ZEROS OR -811
BI0810        PERFORM 2510-VERIFICA-FUTURO
           END-IF.

      *----------------------------------------------------------------*
       2400-7-99-FIM.                  
           EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2400-8-VERIFICAR-UF SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB001
              TO CAGPTO-CTA OF GFCTB008.
           MOVE CSERVC-TARIF OF GFCTB001
              TO CSERVC-TARIF OF GFCTB008.
           MOVE DINIC-ADSAO-COMP OF GFCTB001
              TO DINIC-ADSAO-COMP OF GFCTB008.
           MOVE GFCTLH-CUF TO CSGL-UF OF GFCTB008.

           EXEC SQL
             SELECT
                   CSGL_UF
             INTO
                   :GFCTB008.CSGL-UF
             FROM DB2PRD.ADSAO_GRP_UF
             WHERE
                   CAGPTO_CTA = :GFCTB008.CAGPTO-CTA AND
                   DINIC_ADSAO_COMP = :GFCTB008.DINIC-ADSAO-COMP AND
                   CSGL_UF = :GFCTB008.CSGL-UF
           END-EXEC.

BI0810     IF (SQLCODE NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0011' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'TADSAO_GRP_UF   ' TO GFCT0M-NOME-TAB
              MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
              MOVE '0440' TO GFCT0M-LOCAL
              MOVE 0011 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB008' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

BI0810     IF SQLCODE EQUAL ZEROS OR -811
BI0810        PERFORM 2510-VERIFICA-FUTURO
           END-IF.

      *----------------------------------------------------------------*
       2400-8-99-FIM.                  
           EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2400-14-FECHAR-CURSOR SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                   CLOSE CSR02-GFCTB001
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0011' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'ADSAO_COMP_PCOTE' TO GFCT0M-NOME-TAB
              MOVE 'CLOSE' TO GFCT0M-COMANDO-SQL
              MOVE '0450' TO GFCT0M-LOCAL
              MOVE 0011 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB001' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2400-14-99-FIM.                 
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    MOVER DADOS PARA TABELA GFCTB001                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2410-INSERIR-DADOS-GFCTB001 SECTION.
      *----------------------------------------------------------------*

           PERFORM 2411-OBTER-MAIOR-SEQUENCIA.

           MOVE GFCTLH-CAGPTO TO CAGPTO-CTA OF GFCTB001.
           MOVE GFCTLH-CPACOTE TO CSERVC-TARIF OF GFCTB001.
           MOVE WRK-SEQUENCIA TO CSEQ-AGPTO-CTA OF GFCTB001.
           MOVE GFCTLH-DATA-INICIO TO DINIC-ADSAO-COMP OF GFCTB001.
           MOVE GFCTLH-DATA-FIM TO DFIM-ADSAO-COMP OF GFCTB001.
BI0810     MOVE GFCTLH-CPSSOA-TARIFA TO CPSSOA-SERVC-TARIF OF GFCTB001.
           MOVE GFCTLH-FUNC-BDSCO TO WRK-FUNC-BDSCO.
           MOVE WRK-FUNC-BDSCO-N TO CFUNC-MANUT-INCL OF GFCTB001.
           MOVE ZEROS TO CFUNC-MANUT-EXCL OF GFCTB001.

           IF GFCTLH-TOTAL EQUAL SPACES
              MOVE 'N' TO CINDCD-AGPTO-TOT OF GFCTB001
           ELSE
              MOVE GFCTLH-TOTAL TO CINDCD-AGPTO-TOT OF GFCTB001
           END-IF.

           MOVE '0001-01-01-01.01.01.000001'
              TO HMANUT-REG OF GFCTB001.

           PERFORM 2412-INSERIR-GFCTB001.

      *----------------------------------------------------------------*
       2410-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*


      ******************************************************************
      *    OBTER MAIOR FREQUENCIA                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2411-OBTER-MAIOR-SEQUENCIA SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLH-CAGPTO TO CAGPTO-CTA OF GFCTB001.
           MOVE GFCTLH-CPACOTE TO CSERVC-TARIF OF GFCTB001.
           MOVE GFCTLH-DATA-INICIO TO DINIC-ADSAO-COMP OF GFCTB001.

           EXEC SQL
             SELECT
                   MAX(CSEQ_AGPTO_CTA)
             INTO :GFCTB001.CSEQ-AGPTO-CTA
             FROM DB2PRD.ADSAO_COMP_PCOTE
             WHERE CAGPTO_CTA = :GFCTB001.CAGPTO-CTA
              AND CSERVC_TARIF = :GFCTB001.CSERVC-TARIF
              AND DINIC_ADSAO_COMP = :GFCTB001.DINIC-ADSAO-COMP
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0012' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'ADSAO_COMP_PCOTE' TO GFCT0M-NOME-TAB
              MOVE 'SELECT' TO GFCT0M-COMANDO-SQL
              MOVE '0460' TO GFCT0M-LOCAL
              MOVE 0012 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB001' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

           IF SQLCODE EQUAL -305
              MOVE 1 TO WRK-SEQUENCIA
           ELSE
              COMPUTE WRK-SEQUENCIA EQUAL
                 (CSEQ-AGPTO-CTA OF GFCTB001 + 1)
           END-IF.

      *----------------------------------------------------------------*
       2411-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    INSERE REGISTRO NA TABELA GFCTB001                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2412-INSERIR-GFCTB001 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             INSERT INTO DB2PRD.ADSAO_COMP_PCOTE
                  (CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
                   DFIM_ADSAO_COMP,
                   CFUNC_MANUT_INCL,
                   CFUNC_MANUT_EXCL,
                   CINDCD_AGPTO_TOT,
                   HINCL_REG_SIST,
                   HMANUT_REG,
BI0810              CPSSOA_SERVC_TARIF)
               VALUES
                  (:GFCTB001.CSERVC-TARIF,
                   :GFCTB001.CAGPTO-CTA,
                   :GFCTB001.CSEQ-AGPTO-CTA,
                   :GFCTB001.DINIC-ADSAO-COMP,
                   :GFCTB001.DFIM-ADSAO-COMP,
                   :GFCTB001.CFUNC-MANUT-INCL,
                   :GFCTB001.CFUNC-MANUT-EXCL,
                   :GFCTB001.CINDCD-AGPTO-TOT,
                   CURRENT TIMESTAMP,
                   :GFCTB001.HMANUT-REG,
BI0810              :GFCTB001.CPSSOA-SERVC-TARIF)
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0003' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'ADSAO_COMP_PCOTE' TO GFCT0M-NOME-TAB
              MOVE 'INSERT' TO GFCT0M-COMANDO-SQL
              MOVE '0470' TO GFCT0M-LOCAL
              MOVE 0003 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB001' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2412-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    INSERE REGISTRO PAB NA TABELA GFCTB005                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2420-INSERIR-PAB SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLH-CAGPTO TO CAGPTO-CTA OF GFCTB005.
           MOVE GFCTLH-CPACOTE TO CSERVC-TARIF OF GFCTB005.
           MOVE GFCTLH-DATA-INICIO TO DINIC-ADSAO-COMP OF GFCTB005.
           MOVE WRK-SEQUENCIA TO CSEQ-AGPTO-CTA OF GFCTB005.
           MOVE 237 TO CEMPR-INC OF GFCTB005.
           MOVE GFCTLH-CAGENCIA TO CDEPDC OF GFCTB005.
           MOVE GFCTLH-CPAB TO CPOSTO-SERVC OF GFCTB005.

           EXEC SQL
             INSERT INTO DB2PRD.ADSAO_GRP_PAB
                  (CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC)
               VALUES
                  (:GFCTB005.CSERVC-TARIF,
                   :GFCTB005.CAGPTO-CTA,
                   :GFCTB005.CSEQ-AGPTO-CTA,
                   :GFCTB005.DINIC-ADSAO-COMP,
                   :GFCTB005.CEMPR-INC,
                   :GFCTB005.CDEPDC,
                   :GFCTB005.CPOSTO-SERVC)
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0003' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'ADSAO_GRP_PAB' TO GFCT0M-NOME-TAB
              MOVE 'INSERT' TO GFCT0M-COMANDO-SQL
              MOVE '0480' TO GFCT0M-LOCAL
              MOVE 0003 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB005' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2420-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    INSERE REGISTRO AGENCIA NA TABELA GFCTB002                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2430-INSERIR-AGENCIA SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLH-CAGPTO TO CAGPTO-CTA OF GFCTB002.
           MOVE GFCTLH-CPACOTE TO CSERVC-TARIF OF GFCTB002.
           MOVE GFCTLH-DATA-INICIO TO DINIC-ADSAO-COMP OF GFCTB002.
           MOVE WRK-SEQUENCIA TO CSEQ-AGPTO-CTA OF GFCTB002.
           MOVE 237 TO CEMPR-INC OF GFCTB002.
           MOVE GFCTLH-CAGENCIA TO CDEPDC OF GFCTB002.

           EXEC SQL
             INSERT INTO DB2PRD.ADSAO_GRP_DEPDC
                  (CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
                   CEMPR_INC,
                   CDEPDC)
               VALUES
                  (:GFCTB002.CSERVC-TARIF,
                   :GFCTB002.CAGPTO-CTA,
                   :GFCTB002.CSEQ-AGPTO-CTA,
                   :GFCTB002.DINIC-ADSAO-COMP,
                   :GFCTB002.CEMPR-INC,
                   :GFCTB002.CDEPDC)
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0003' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'ADSAO_GRP_DEPDC' TO GFCT0M-NOME-TAB
              MOVE 'INSERT' TO GFCT0M-COMANDO-SQL
              MOVE '0490' TO GFCT0M-LOCAL
              MOVE 0003 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB002' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2430-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    INSERE REGISTRO EMPRESA NA TABELA GFCTB003                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2440-INSERIR-EMPRESA SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLH-CAGPTO TO CAGPTO-CTA OF GFCTB003.
           MOVE GFCTLH-CPACOTE TO CSERVC-TARIF OF GFCTB003.
           MOVE GFCTLH-DATA-INICIO TO DINIC-ADSAO-COMP OF GFCTB003.
           MOVE WRK-SEQUENCIA TO CSEQ-AGPTO-CTA OF GFCTB003.
           MOVE GFCTLH-CAGENCIA TO CAG-EMPR OF GFCTB003.
           MOVE GFCTLH-CCONTA TO CNRO-CTA-DEB OF GFCTB003.
           MOVE ZEROS TO CRZ-CTA-EMPR OF GFCTB003
                         CLCTO-EMPR OF GFCTB003.

           EXEC SQL
             INSERT INTO DB2PRD.ADSAO_GRP_EMPR
                  (CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
                   CAG_EMPR,
                   CRZ_CTA_EMPR,
                   CNRO_CTA_DEB,
                   CLCTO_EMPR)
               VALUES
                  (:GFCTB003.CSERVC-TARIF,
                   :GFCTB003.CAGPTO-CTA,
                   :GFCTB003.CSEQ-AGPTO-CTA,
                   :GFCTB003.DINIC-ADSAO-COMP,
                   :GFCTB003.CAG-EMPR,
                   :GFCTB003.CRZ-CTA-EMPR,
                   :GFCTB003.CNRO-CTA-DEB,
                   :GFCTB003.CLCTO-EMPR)
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0003' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'ADSAO_GRP_EMPR' TO GFCT0M-NOME-TAB
              MOVE 'INSERT' TO GFCT0M-COMANDO-SQL
              MOVE '0500' TO GFCT0M-LOCAL
              MOVE 0003 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB003' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2440-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    INSERE REGISTRO POSTAL NA TABELA GFCTB006                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2450-INSERIR-POSTAL-EXP SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLH-CAGPTO TO CAGPTO-CTA OF GFCTB006.
           MOVE GFCTLH-CPACOTE TO CSERVC-TARIF OF GFCTB006.
           MOVE GFCTLH-DATA-INICIO TO DINIC-ADSAO-COMP OF GFCTB006.
           MOVE WRK-SEQUENCIA TO CSEQ-AGPTO-CTA OF GFCTB006.
           MOVE 237 TO CEMPR-INC OF GFCTB006.
           MOVE GFCTLH-CAGENCIA TO CDEPDC OF GFCTB006.
           MOVE GFCTLH-CPAB TO CPOSTO-SERVC OF GFCTB006.

           EXEC SQL
             INSERT INTO DB2PRD.ADSAO_GRP_PSTAL
                  (CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC)
               VALUES
                  (:GFCTB006.CSERVC-TARIF,
                   :GFCTB006.CAGPTO-CTA,
                   :GFCTB006.CSEQ-AGPTO-CTA,
                   :GFCTB006.DINIC-ADSAO-COMP,
                   :GFCTB006.CEMPR-INC,
                   :GFCTB006.CDEPDC,
                   :GFCTB006.CPOSTO-SERVC)
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0003' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'ADSAO_GRP_PSTAL' TO GFCT0M-NOME-TAB
              MOVE 'INSERT' TO GFCT0M-COMANDO-SQL
              MOVE '0510' TO GFCT0M-LOCAL
              MOVE 0003 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB006' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2450-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    INSERE REGISTRO CLIENTE NA TABELA GFCTB0H9                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2460-INSERIR-CLIENTE SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLH-CAGPTO TO CAGPTO-CTA OF GFCTB0H9.
           MOVE GFCTLH-CPACOTE TO CSERVC-TARIF OF GFCTB0H9.
           MOVE GFCTLH-DATA-INICIO TO DINIC-ADSAO-COMP OF GFCTB0H9.
           MOVE WRK-SEQUENCIA TO CSEQ-AGPTO-CTA OF GFCTB0H9.
ST2506*    M0VE GFCTLH-CGC-CPF TO CCGC-CPF OF GFCTB0H9.
ST2506*    M0VE GFCTLH-FILIAL TO CFLIAL-CGC OF GFCTB0H9.
ST2506*    MOVE GFCTLH-CONTROLE TO CCTRL-CPF-CGC OF GFCTB0H9. 
ST2506     MOVE GFCTLH-CGC-CPF TO CCGC-CPF-ST OF GFCTB0H9.
ST2506     MOVE GFCTLH-FILIAL TO CFLIAL-CGC-ST OF GFCTB0H9.
           MOVE GFCTLH-CONTROLE TO CCTRL-CPF-CGC-ST OF GFCTB0H9. 
      
           EXEC SQL
             INSERT INTO DB2PRD.TADSAO_GRP_CLI
                  (CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
                   CCGC_CPF,
                   CCTRL_CPF_CGC,
                   CFLIAL_CGC,
                   CCGC_CPF_ST,
                   CCTRL_CPF_CGC_ST,
                   CFLIAL_CGC_ST)
               VALUES
                  (:GFCTB0H9.CSERVC-TARIF,
                   :GFCTB0H9.CAGPTO-CTA,
                   :GFCTB0H9.CSEQ-AGPTO-CTA,
                   :GFCTB0H9.DINIC-ADSAO-COMP,
                   :GFCTB0H9.CCGC-CPF,
                   :GFCTB0H9.CCTRL-CPF-CGC,
                   :GFCTB0H9.CFLIAL-CGC,
                   :GFCTB0H9.CCGC-CPF-ST,
                   :GFCTB0H9.CCTRL-CPF-CGC-ST,
                   :GFCTB0H9.CFLIAL-CGC-ST)
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0003' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'TADSAO_GRP_CLI' TO GFCT0M-NOME-TAB
              MOVE 'INSERT' TO GFCT0M-COMANDO-SQL
              MOVE '0520' TO GFCT0M-LOCAL
              MOVE 0003 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB0H9' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2460-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    INSERE REGISTRO SEGMENTO NA TABELA GFCTB007                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2470-INSERIR-SEGMENTO SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLH-CAGPTO TO CAGPTO-CTA OF GFCTB007.
           MOVE GFCTLH-CPACOTE TO CSERVC-TARIF OF GFCTB007.
           MOVE GFCTLH-DATA-INICIO TO DINIC-ADSAO-COMP OF GFCTB007.
           MOVE WRK-SEQUENCIA TO CSEQ-AGPTO-CTA OF GFCTB007.
           MOVE GFCTLH-CSEGMENTO TO CSGMTO-GSTAO-TARIF
              OF GFCTB007.

           EXEC SQL
             INSERT INTO DB2PRD.ADSAO_GRP_SGMTO
                  (CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
                   CSGMTO_GSTAO_TARIF)
               VALUES
                  (:GFCTB007.CSERVC-TARIF,
                   :GFCTB007.CAGPTO-CTA,
                   :GFCTB007.CSEQ-AGPTO-CTA,
                   :GFCTB007.DINIC-ADSAO-COMP,
                   :GFCTB007.CSGMTO-GSTAO-TARIF)
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0003' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'ADSAO_GRP_SGMTO' TO GFCT0M-NOME-TAB
              MOVE 'INSERT' TO GFCT0M-COMANDO-SQL
              MOVE '0530' TO GFCT0M-LOCAL
              MOVE 0003 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB007' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2470-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    INSERE REGISTRO MUNICIPIO NA TABELA GFCTB004                *
      ******************************************************************
      *----------------------------------------------------------------*
       2480-INSERIR-MUNICIPIO SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLH-CAGPTO TO CAGPTO-CTA OF GFCTB004.
           MOVE GFCTLH-CPACOTE TO CSERVC-TARIF OF GFCTB004.
           MOVE GFCTLH-DATA-INICIO TO DINIC-ADSAO-COMP OF GFCTB004.
           MOVE WRK-SEQUENCIA TO CSEQ-AGPTO-CTA OF GFCTB004.
           MOVE GFCTLH-CMUNIC TO CMUN-IBGE OF GFCTB004.

           EXEC SQL
             INSERT INTO DB2PRD.ADSAO_GRP_MUN
                  (CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
                   CMUN_IBGE)
               VALUES
                  (:GFCTB004.CSERVC-TARIF,
                   :GFCTB004.CAGPTO-CTA,
                   :GFCTB004.CSEQ-AGPTO-CTA,
                   :GFCTB004.DINIC-ADSAO-COMP,
                   :GFCTB004.CMUN-IBGE)
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0003' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'ADSAO_GRP_MUN' TO GFCT0M-NOME-TAB
              MOVE 'INSERT' TO GFCT0M-COMANDO-SQL
              MOVE '0540' TO GFCT0M-LOCAL
              MOVE 0003 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB004' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2480-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    INSERE REGISTRO UF NA TABELA GFCTB008                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2490-INSERIR-UF SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLH-CAGPTO TO CAGPTO-CTA OF GFCTB008.
           MOVE GFCTLH-CPACOTE TO CSERVC-TARIF OF GFCTB008.
           MOVE GFCTLH-DATA-INICIO TO DINIC-ADSAO-COMP OF GFCTB008.
           MOVE WRK-SEQUENCIA TO CSEQ-AGPTO-CTA OF GFCTB008.
           MOVE GFCTLH-CUF TO CSGL-UF OF GFCTB008.

           EXEC SQL
             INSERT INTO DB2PRD.ADSAO_GRP_UF
                  (CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
                   CSGL_UF)
               VALUES
                  (:GFCTB008.CSERVC-TARIF,
                   :GFCTB008.CAGPTO-CTA,
                   :GFCTB008.CSEQ-AGPTO-CTA,
                   :GFCTB008.DINIC-ADSAO-COMP,
                   :GFCTB008.CSGL-UF)
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS) OR
              (SQLWARN0 EQUAL 'W')
              MOVE SPACES TO GFCT0M-AREA-ERROS
              INITIALIZE GFCT0M-AREA-ERROS
              MOVE '0003' TO GFCTG2-COD-MSG
              PERFORM 1210-OBTER-DESC-MSG
              MOVE 'ADSAO_GRP_UF' TO GFCT0M-NOME-TAB
              MOVE 'INSERT' TO GFCT0M-COMANDO-SQL
              MOVE '0550' TO GFCT0M-LOCAL
              MOVE 0003 TO GFCTLI-COD-MSG-ERRO
              MOVE ' - GFCTB008' TO WRK-NOME-TAB
              PERFORM 1321-MOVER-ERRO-SQL
              MOVE SPACES TO GFCTLI-DADOS-RST
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2490-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

BI0810******************************************************************
BI0810*VERIFICA DUPLICADO                                              *
BI0810******************************************************************
BI0810*----------------------------------------------------------------*
BI0810 2500-VERIFICA-DUPLICADO SECTION.
BI0810*----------------------------------------------------------------*
BI0810
BI0810     IF (CPSSOA-SERVC-TARIF OF GFCTB001 EQUAL '3') OR
BI0810        (GFCTLH-CPSSOA-TARIFA EQUAL '3')
BI0810        MOVE 'S' TO WRK-ACHOU-DUPLICIDADE
BI0810     ELSE
BI0810        IF GFCTLH-CPSSOA-TARIFA EQUAL CPSSOA-SERVC-TARIF
BI0810           OF GFCTB001
BI0810           MOVE 'S' TO WRK-ACHOU-DUPLICIDADE
BI0810        END-IF
BI0810     END-IF.
BI0810
BI0810*----------------------------------------------------------------*
BI0810 2500-99-FIM.                    
           EXIT.
BI0810*----------------------------------------------------------------*
BI0810
BI0810******************************************************************
BI0810*VERIFICA FUTURO                                                 *
BI0810******************************************************************
BI0810*----------------------------------------------------------------*
BI0810 2510-VERIFICA-FUTURO SECTION.
BI0810*----------------------------------------------------------------*
BI0810
BI0810     IF (CPSSOA-SERVC-TARIF OF GFCTB001 EQUAL '3') OR
BI0810        (GFCTLH-CPSSOA-TARIFA EQUAL '3')
BI0810        MOVE 'S' TO WRK-ACHOU-FUTURO
BI0810     ELSE
BI0810        IF GFCTLH-CPSSOA-TARIFA EQUAL CPSSOA-SERVC-TARIF
BI0810           OF GFCTB001
BI0810           MOVE 'S' TO WRK-ACHOU-FUTURO
BI0810        END-IF
BI0810     END-IF.
BI0810
BI0810*----------------------------------------------------------------*
BI0810 2510-99-FIM.                    
           EXIT.
BI0810*----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA FINALIZAR PROGRAMA                              *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*