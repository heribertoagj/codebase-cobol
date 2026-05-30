      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0131.
       AUTHOR.     RICARDO PEREIRA DA SILVA.
      *================================================================
      *              C P M  B R A X I S  -  S I S T E M A S            *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  GFCT0131                                     *
      *    PROGRAMADOR :  RICARDO PEREIRA DA SILVA - PROCWORK - GP50   *
      *    ANALISTA....:  RICARDO PEREIRA DA SILVA - PROCWORK - GP50   *
      *    DATA........:  02/07/2008                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  ADESAO INDIVIDUAL - CONSULTA DETALHADA.      *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                        INCLUDE/BOOK     *
      *                    DB2PRD.ADSAO_INDVD_PCOTE     GFCTB009       *
      *                    DB2PRD.SERVC_TARIF_PRINC     GFCTB0D8       *
1911  *                    DB2PRD.TADSAO_INDVD_CLULR    GFCTB0M2       *
MAR10 *                    DB2PRD.TOPER_FONE_CREDT      GFCTB0M4       *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTE0 - AREA DE COMUNICACAO - ENTRADA.                   *
      *    I#GFCTE1 - AREA DE COMUNICACAO - SAIDA.                     *
      *    I#GFCTG2 - AREA DE COMUNICACAO - ENTRADA DO MOD. GFCT5523.  *
      *    I#GFCTG3 - AREA DE COMUNICACAO - SAIDA   DO MOD. GFCT5523.  *
      *    I#GFCTFZ - AREA DE COMUNICACAO - ENTRADA DO MOD. GFCT5522.  *
      *    I#GFCTG1 - AREA DE COMUNICACAO - SAIDA   DO MOD. GFCT5522.  *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    GFCT5522 - VERIFICA DISPONIBILIDADE ON LINE.                *
      *    GFCT5006 - OBTER DESCRICAO DA AGENCIA.                      *
      *    GFCT5054 - OBTER DADOS DO CLIENTE.                          *
      *    SARH8000 - PERMITIR AOS SISTEMAS LEGADOS ACESSO AS INFORMA- *
      *               COES DE FUNCIONARIOS NO REPOSITORIO DE DADOS SAP *
      *================================================================*
      *               ULTIMA ALTERACAO EM 27/03/2009                   *
      *    RESPONSAVEL : HELIO SANTONI                                 *
      *  - INCLUIR TRATAMENTO PARA MOSTRAR CASOS DE EXCLUSAO MASSIVA   *
      *    DE ADESAO INDIVIDUAL.                                       *
1911  *----------------------------------------------------------------*
1911  *        SONDA PROCWORK - CONSULTORIA - ALTERACAO - SP0705       *
1911  *----------------------------------------------------------------*
1911  *                                                                *
1911  *    PROGRAMADOR.:  HELIO SANTONI               - SONDA/PROCWORK *
1911  *    DATA........:  19/11/2009                                   *
1911  *    OBJETIVO....:  INCLUIR TRATAMENTO DA TABELA GFCTB0M2.       *
1911  *                                                                *
      *================================================================*
MAR10 *----------------------------------------------------------------*
MAR10 *                 U L T I M A   A L T E R A C A O                *
MAR10 *----------------------------------------------------------------*
MAR10 *        SONDA PROCWORK - CONSULTORIA - ALTERACAO                *
MAR10 *----------------------------------------------------------------*
MAR10 *                                                                *
MAR10 *    PROGRAMADOR.:  FABRICA                     - SONDA/PROCWORK *
MAR10 *    DATA........:  MAR/2010                                     *
MAR10 *    OBJETIVO....:  OBTER DESCRICAO DA OPERADORA NA TABELA DB2   *
MAR10 *                   GFCTB0M4 AO INVES DO COPY GFCTWAJJ.          *
MAR10 *================================================================*
JUN10 *----------------------------------------------------------------*
JUN10 *                 U L T I M A   A L T E R A C A O                *
JUN10 *----------------------------------------------------------------*
JUN10 *        SONDA PROCWORK - CONSULTORIA - ALTERACAO                *
JUN10 *----------------------------------------------------------------*
JUN10 *                                                                *
JUN10 *    PROGRAMADOR.:  FABRICA                     - SONDA/PROCWORK *
JUN10 *    DATA........:  JUN/2010                                     *
JUN10 *    OBJETIVO....:  IDENTIFICACAO DOS MODULOS DO CORRESPONDENTE  *
JUN10 *                   NAO BANCARIO PARA DESCRICAO DO CANAL E       *
JUN10 *                   TAMBEM OS NOVOS MODULOS DO FONE FACIL        *
JUN10 *================================================================*
MAI11 *----------------------------------------------------------------*
MAI11 *                 U L T I M A   A L T E R A C A O                *
MAI11 *----------------------------------------------------------------*
MAI11 *        SONDA PROCWORK - CONSULTORIA - ALTERACAO                *
MAI11 *----------------------------------------------------------------*
MAI11 *                                                                *
MAI11 *    PROGRAMADOR.:  FABRICA                     - SONDA/PROCWORK *
MAI11 *    ANALISTA....:  REGINA YAMASHITA            - SONDA/PROCWORK *
MAI11 *    DATA........:  MAI/2011                                     *
MAI11 *    OBJETIVO....:  PREVER NOVO CODIGO DE PROGRAMA 2453          *
MAI11 *================================================================*
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
VI0816*----------------------------------------------------------------*
VI0816*                 U L T I M A   A L T E R A C A O                *
VI0816*----------------------------------------------------------------*
VI0816*        SONDA IT  - ALTERACAO - VI0816                          *
VI0816*----------------------------------------------------------------*
VI0816*                                                                *
VI0816*    ANALISTA....:  VINICIUS        - SONDA IT                   *
VI0816*    DATA........:  08 / 2016                                    *
VI0816*    OBJETIVO....:  INCLUSAO DE DEMONSTRACAO - CANAL             *
VI0816*                          BRADESCO NEXT.                        *
VI0816*                                                                *
      *================================================================*
VI0517*----------------------------------------------------------------*
VI0517*                 U L T I M A   A L T E R A C A O                *
VI0517*----------------------------------------------------------------*
VI0517*        WIPRO-INFOSERVER - ALTERACAO - VI0517                   *
VI0517*----------------------------------------------------------------*
VI0517*                                                                *
VI0517*    ANALISTA....:  VINICIUS        - WIPRO INFOSERVER           *
VI0517*    DATA........:  05 / 2017                                    *
VI0517*    OBJETIVO....:  CORRECAO DO PROCESSO DE ADESAO ALIMENTADO    *
VI0517*                    VIA PROGRAMA GFCT2453 E INCLUSAO DA         *
VI0517*                    IDENTIFICACAO DA ENTRADA VIA CMTF - 9999206 *
      *================================================================*
220125*----------------------------------------------------------------*
.     *                    A L T E R A C A O                           *
.     *----------------------------------------------------------------*
.     *        ACT DIGITAL - CONSULTORIA - ALTERACAO                   *
.     *----------------------------------------------------------------*
.     *                                                                *
.     *    PROGRAMADOR.:  ALEXANDRE PALADINO - ACT DIGITAL             *
.     *    ANALISTA....:  WELLINGTON BATISTA - ACT DIGITAL             *
.     *    DATA........:  10/01/2025                                   *
.     *    OBJETIVO....:  INCLUIR USUARIO PARA CONSULTA DE ADESAO      *
.     *                   INCLUIDA NO TRAG.                            *
.     *                                                                *
220125*================================================================*

      *----------------------------------------------------------------*
      *                    A L T E R A C A O                           *
      *----------------------------------------------------------------*
      *    PROGRAMADOR.:  ESTER COSTA - BRADESCO - EC01                *
      *    ANALISTA....:  ESTER COSTA - BRADESCO                       *
      *    DATA........:  16/04/2025                                   *
      *    OBJETIVO....:  INCLUIR TRATAMENTO P USUARIO DE CANCELAMENTO *
      *                   DE ADESAO UTILIZADO PELO BRAD EXPRESSO       *
      *                   PGM GOTF3RAE, POIS PGM E CHAMADO POR USUARIO *
      *                   SISTEMICO                                    *
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
           'AREA PARA VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'AREA DE VARIAVEIS DE REDEFINICAO'.
      *----------------------------------------------------------------*

1119   01  WRK-AUX-3-S                 PIC S9(003)         VALUE ZEROS.
1119   01  FILLER                      REDEFINES           WRK-AUX-3-S.
1119       05  FILLER                  PIC  9(001).
1119       05  WRK-AUX-2               PIC  9(002).
1119
022012 01  WRK-AUX-11-S                PIC S9(011)         VALUE ZEROS.
022012 01  FILLER                      REDEFINES          WRK-AUX-11-S.
022012     05  WRK-AUX-11              PIC  9(011).
1119
1119   01  WRK-AUX-9-S                 PIC S9(009)         VALUE ZEROS.
1119   01  FILLER                      REDEFINES           WRK-AUX-9-S.
1119       05  WRK-AUX-9               PIC  9(009).

       01  WRK-AUXILIAR-4              PIC +9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES        WRK-AUXILIAR-4.
           05  FILLER                  PIC  X(001).
           05  FILLER                  PIC  9(006).
           05  WRK-AUX-4               PIC  9(003).

       01  WRK-MSG-AUX.
           05 WRK-MSG-75               PIC  X(075)         VALUE SPACES.
           05 FILLER                   PIC  X(004)         VALUE SPACES.

VI0816 01  WRK-CONTROLE-X.
VI0816     05 WRK-CONTROLE-9           PIC  9(002)         VALUE ZEROES.

       01  WRK-DATA-HORA.
           03  WRK-TIMESTAMP.
               05  WRK-ANO             PIC  9(04)       VALUE ZEROS.
               05  FILLER              PIC  X(01)       VALUE '-'.
               05  WRK-MES             PIC  9(02)       VALUE ZEROS.
               05  FILLER              PIC  X(01)       VALUE '-'.
               05  WRK-DIA             PIC  9(02)       VALUE ZEROS.
               05  FILLER              PIC  X(01)       VALUE '-'.
               05  WRK-HORA            PIC  9(02)       VALUE ZEROS.
               05  FILLER              PIC  X(01)       VALUE '.'.
               05  WRK-MIN             PIC  9(02)       VALUE ZEROS.
               05  FILLER              PIC  X(01)       VALUE '.'.
               05  WRK-SEG             PIC  9(02)       VALUE ZEROS.
               05  FILLER              PIC  X(01)       VALUE '.'.
               05  WRK-NAN             PIC  9(06)       VALUE ZEROS.

       01  WRK-DATA-DB2.
           05  WRK-DIA-DB2         PIC  9(02)       VALUE ZEROS.
           05  FILLER              PIC  X(01)       VALUE '.'.
           05  WRK-MES-DB2         PIC  9(02)       VALUE ZEROS.
           05  FILLER              PIC  X(01)       VALUE '.'.
           05  WRK-ANO-DB2         PIC  9(04)       VALUE ZEROS.

       01  WRK-TIME-X.
           05  WRK-HORA-X          PIC  9(02)       VALUE ZEROS.
           05  FILLER              PIC  X(01)       VALUE '.'.
           05  WRK-MINU-X          PIC  9(02)       VALUE ZEROS.

       01  WRK-DESC-FUNCIONARIO        PIC  X(040)        VALUE SPACES.

       01  WRK-COD-FUNC                PIC 9(009)        VALUE ZEROS.
       01  WRK-COD-FUNC-R              REDEFINES WRK-COD-FUNC.
           05  FILLER                  PIC  X(002).
           05  WRK-COD-FUNC-X7          PIC  X(007).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA MODULO SARH8000 ***'.
      *----------------------------------------------------------------*

       01  WRK-ENTRADA.
           03  WRK-PESQUISA-ENT        PIC  9(009)         VALUE ZEROS.
           03  WRK-TAMANHO-ENT         PIC  9(004)         VALUE ZEROS.
           03  WRK-SOLICITA-ENT.
               05  FILLER              OCCURS 255 TIMES.
                   07  WRK-CAMPO-ENT   PIC  9(003)         VALUE ZEROS.

       01  WRK-SAIDA.
           03  WRK-COD-RETORNO-SAI     PIC  9(002)         VALUE ZEROS.
           03  WRK-TAMANHO-SAI         PIC  9(004)         VALUE ZEROS.
           03  WRK-REGISTRO-SAI.
               05  FILLER              OCCURS 0 TO 1000 TIMES
                   DEPENDING ON WRK-TAMANHO-SAI.
                   07  WRK-CONTADOR-SAI PIC  X(001)        VALUE SPACES.

       01  WRK-ERRO-AREA               PIC  X(107)         VALUE SPACES.
       01  WRK-SQLCA                   PIC  X(136)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE VARIAVEIS DE MENSAGEM'.
      *----------------------------------------------------------------*

       01  WRK-MSG01.
           05  FILLER                  PIC  X(026)         VALUE
               'ERRO NA CHAMADA DO MODULO'.
           05  WRK-NOME-MOD            PIC  X(008)         VALUE SPACES.

       01  WRK-NOME-TAB                PIC  X(011)         VALUE SPACES.

       01  WRK-MSG02                   PIC  X(026)         VALUE
           'SISTEMA INDISPONIVEL'.

       01  WRK-MSG05.
           05  WRK-MSG05-MODULO        PIC  X(08)          VALUE SPACES.
           05  FILLER                  PIC  X(67)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE ACESSO AO MODULO GFCT5523'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE ACESSO AO MODULO GFCT5522'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTFZ'.

       COPY 'I#GFCTG1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)         VALUE
           '*    AREA DO MODULO GFCT5006   *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCT1V'.

       COPY 'I#GFCT1X'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE ACESSO AO MODULO GFCT5054'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG0'.

       COPY 'I#GFCTHA'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(020)         VALUE
           'AREA PARA DB2'.
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

1911       EXEC SQL
1911           INCLUDE GFCTB0M2
1911       END-EXEC.

MAR10      EXEC SQL
MAR10        INCLUDE GFCTB0M4
MAR10      END-EXEC.
MAR10
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA CURSORES *'.
      *----------------------------------------------------------------*

1119       EXEC SQL
1119         DECLARE CSR01-GFCTB0M2 CURSOR  FOR
1119         SELECT
1119                 NSEQ_REG_CLULR,
1119                 COPER_FONE_MOVEL,
1119                 CDDD_FONE_MOVEL,
022012               NLIN_TFONI
1119         FROM
1119                 DB2PRD.TADSAO_INDVD_CLULR
1119         WHERE
1119                 CAG_BCRIA        = :GFCTB0M2.CAG-BCRIA
1119           AND   CCTA_CLI         = :GFCTB0M2.CCTA-CLI
1119           AND   CSERVC_TARIF     = :GFCTB0M2.CSERVC-TARIF
1119           AND   HINCL_REG        = :GFCTB0M2.HINCL-REG
1119         ORDER BY
1119                 NSEQ_REG_CLULR DESC
1119       END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(030)         VALUE
           'FIM DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTE0'.

       COPY 'I#GFCTE1'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTE0-ENTRADA
                                                      GFCTE1-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *    FLUXO DE PROCESSAMENTO DO PROGRAMA.                         *
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
      *    ROTINA QUE CHAMA OS PROCEDIMENTOS INICIAIS DO PROGRAMA.     *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO  GFCT0M-AREA-ERROS
                                           GFCTE1-SAIDA.

           INITIALIZE                  GFCTE1-SAIDA
                                       GFCT0M-AREA-ERROS.

           PERFORM 1100-INICIALIZAR-AREA-SAIDA.

           PERFORM 1200-VERIFICAR-SIST-DISPONIVEL.

           PERFORM 1300-EFETUAR-CONSISTENCIA.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    INICILIZA AS AREAS DE SAIDA                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-INICIALIZAR-AREA-SAIDA     SECTION.
      *----------------------------------------------------------------*

           MOVE 1010                   TO GFCTE1-LL.
           MOVE ZEROS                  TO GFCTE1-ZZ

           MOVE GFCTE0-TRANSACAO       TO GFCTE1-TRANSACAO.
           MOVE GFCTE0-FUNCAO          TO GFCTE1-FUNCAO.
           MOVE GFCTE0-FUNC-BDSCO      TO GFCTE1-FUNC-BDSCO.
           MOVE GFCTE0-AGENCIA         TO GFCTE1-AGENCIA.
           MOVE GFCTE0-CONTA           TO GFCTE1-CONTA.
           MOVE GFCTE0-COD-TARIFA      TO GFCTE1-COD-TARIFA.
           MOVE GFCTE0-HINCL-REG       TO GFCTE1-HINCL-REG.
           MOVE GFCTE0-SITUACAO-ADESAO TO GFCTE1-SITUACAO-ADESAO.

           MOVE 'S'                    TO GFCTE1-FIM.

           MOVE ZEROS                  TO GFCTE1-COD-SQL-ERRO
                                          GFCTE1-COD-MSG-ERRO
                                          GFCTE1-ERRO.

           MOVE '0000'                 TO GFCTG2-COD-MSG.

           PERFORM 1110-OBTER-DESC-MENSAGEM.

           MOVE GFCTG3-DESC-MSG-ERRO   TO GFCTE1-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTER A DESCRICAO DA MENSAGEM.                              *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-OBTER-DESC-MENSAGEM        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE                  GFCTG3-SAIDA.

           MOVE 0100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE GFCTE0-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTE0-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTE0-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'N'                    TO GFCTG2-FIM.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO(1:34)
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0131'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTE1-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 9                  TO GFCTE1-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO(1:34)
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0131'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE 1                  TO GFCTE1-ERRO
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO GFCTE1-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO GFCTE1-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO GFCTE1-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *   ROTINA PARA VERIFICACAO DA DISPONIBILIDADE DO SISTEMA        *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-VERIFICAR-SIST-DISPONIVEL   SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG1-SAIDA.

           INITIALIZE                  GFCTG1-SAIDA.

           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE GFCTE0-TRANSACAO       TO GFCTFZ-TRANSACAO.
           MOVE GFCTE0-FUNCAO          TO GFCTFZ-FUNCAO.
           MOVE GFCTE0-FUNC-BDSCO      TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.
           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL  WRK-MODULO            USING GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTE1-ERRO
               MOVE WRK-MODULO         TO WRK-NOME-MOD
                                          GFCT0M-PROGRAMA
               MOVE WRK-MSG01          TO GFCT0M-TEXTO(1:34)
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0131'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 9
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE GFCTG1-DESC-MSG-ERRO
                                       TO WRK-MSG-AUX
                   MOVE WRK-MSG-75     TO GFCT0M-TEXTO
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
               ELSE
                   MOVE GFCTG1-COD-SQL-ERRO
                                       TO GFCTE1-COD-SQL-ERRO
               END-IF
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'GFCT0131'         TO GFCT0M-TRANSACAO
               MOVE GFCTG1-ERRO        TO GFCTE1-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE WRK-MSG02          TO GFCT0M-TEXTO(1:26)
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0131'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTE1-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CONSISTE A AREA DE ENTRADA.                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-EFETUAR-CONSISTENCIA       SECTION.
      *----------------------------------------------------------------*

           IF (GFCTE0-TRANSACAO        EQUAL SPACES
                                       OR LOW-VALUES) OR
              (GFCTE0-FUNCAO           EQUAL SPACES
                                       OR LOW-VALUES) OR
              (GFCTE0-FUNC-BDSCO       EQUAL SPACES
                                       OR LOW-VALUES) OR
              (GFCTE0-AGENCIA          NOT NUMERIC    OR
               GFCTE0-AGENCIA          EQUAL ZEROS)   OR
              (GFCTE0-CONTA            NOT NUMERIC    OR
               GFCTE0-CONTA            EQUAL ZEROS)   OR
              (GFCTE0-COD-TARIFA       NOT NUMERIC    OR
               GFCTE0-COD-TARIFA       EQUAL ZEROS)   OR
              (GFCTE0-HINCL-REG        EQUAL SPACES   OR LOW-VALUES) OR
              (GFCTE0-SITUACAO-ADESAO  EQUAL SPACES   OR LOW-VALUES)
               MOVE 1                  TO GFCTE1-ERRO
                                          GFCTE1-COD-MSG-ERRO
               MOVE ZEROS              TO GFCTE1-COD-SQL-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTE1-DESC-MSG-ERRO(1:70)
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-ACESSAR-GFCTB009.

           MOVE CJUNC-DEPDC     OF GFCTB009  TO GFCTE1-AGENCIA.

           PERFORM 2300-ACESSAR-GFCT5006.

           MOVE GFCT1X-DESC-DEPDC(1)(1:40)   TO GFCTE1-NOME-AGENCIA.

           MOVE CCTA-CLI        OF GFCTB009  TO GFCTE1-CONTA.

           MOVE CSERVC-TARIF    OF GFCTB009  TO GFCTE1-COD-TARIFA.

           PERFORM 2200-OBTER-DESC-TARIFA.

           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8(1:40)
                                       TO GFCTE1-DESCR-TARIFA.

           MOVE GFCTE0-SITUACAO-ADESAO TO GFCTE1-SITUACAO-ADESAO.

           PERFORM 2400-OBTER-DADOS-CLIENTE.

           MOVE GFCTHA-COD-CGC-CPF            TO  GFCTE1-CPF-CNPJ
           MOVE GFCTHA-COD-FILIAL-CGC         TO  GFCTE1-FILIAL
VI0816     MOVE GFCTHA-COD-CONTROLE-CPF-CGC   TO  WRK-CONTROLE-9
VI0816     MOVE WRK-CONTROLE-X                TO  GFCTE1-CONTROLE
           MOVE GFCTHA-NOME-CLIENTE(1:40)     TO  GFCTE1-NOME-CLI

1911       PERFORM 2600-TRATAR-GFCTB0M2.

           EVALUATE CFUNC-MANUT-INCL OF GFCTB009

                  WHEN 1
                      MOVE 'INDETERMINADO'  TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PGM GFCT1570 OU GFCT6011 OU GFCT9570'
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                      MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

                  WHEN 1570
                      MOVE 'BATCH - CESTA CONTA CORRENTE - A PEDIDO'
                                        TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PROGRAMA GFCT1570'
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                      MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

                  WHEN 1585
                      MOVE 'BATCH - CONTAS NOVAS - MENSAL'
                                        TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PROGRAMA GFCT1585'
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                      MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

                  WHEN 15
                      MOVE 'BATCH - CONTAS NOVAS - MENSAL'
                                        TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PROGRAMA GFCT1585'
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                      MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

                  WHEN 1910
                      MOVE 'BATCH - TRAG'
                                        TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PROGRAMA GFCT1910'
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                      MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

220125            WHEN 2211
.                     MOVE 'BATCH - PRE TRAG'
.                                       TO GFCTE1-CANAL-INCLUSAO
.                     MOVE CFUNC-MANUT-INCL OF GFCTB009
.                                           TO WRK-COD-FUNC
.                     MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
.                     MOVE 'VIA PROGRAMA GFCT2B1A'
.                                       TO GFCTE1-DESC-FUNC-INCL
.                     MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
.                     MOVE WRK-ANO               TO  WRK-ANO-DB2
.                     MOVE WRK-MES               TO  WRK-MES-DB2
.                     MOVE WRK-DIA               TO  WRK-DIA-DB2
.                     MOVE WRK-HORA              TO  WRK-HORA-X
.                     MOVE WRK-MIN               TO  WRK-MINU-X
.                     MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
.                     MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL
220125
                  WHEN 1947
                      MOVE 'BATCH - CONTAS NOVAS - DIARIA'
                                        TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PROGRAMA GFCT1947'
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                      MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

                  WHEN 14
                      MOVE 'BATCH - CONTAS NOVAS - DIARIA'
                                        TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PROGRAMA GFCT1947'
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                      MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

                  WHEN 2108
                      MOVE 'BATCH - CONTAS CANCELADAS-MENSAL'
                                        TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PROGRAMA GFCT2108'
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                      MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

                  WHEN 20
                      MOVE 'BATCH - CONTAS CANCELADAS-MENSAL'
                                        TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PROGRAMA GFCT2108'
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                      MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

                  WHEN 2109
                      MOVE 'BATCH - CANCELAMENTO MASSIVO - A PEDIDO'
                                        TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PROGRAMA GFCT2109'
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                      MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

                  WHEN 25
                      MOVE 'BATCH - CANCELAMENTO MASSIVO - A PEDIDO'
                                        TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PROGRAMA GFCT2109'
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                      MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

                  WHEN 6011
                      MOVE 'MODULO CORPORATIVO - INCLUSAO'
                                        TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PROGRAMA GFCT6011'
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                      MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

                  WHEN 6016
                      MOVE 'MODULO CORPORATIVO - CANCELAMENTO'
                                        TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PROGRAMA GFCT6016'
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                      MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

                  WHEN 9999999
                      MOVE 'MODULO CORPORATIVO - CANCELAMENTO'
                                        TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PROGRAMA GFCT6016'
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                      MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

AGO16             WHEN 6045
AGO16                 MOVE 'MODULO PREABERTURA - INCLUSAO'
AGO16                                   TO GFCTE1-CANAL-INCLUSAO
AGO16                 MOVE CFUNC-MANUT-INCL OF GFCTB009
AGO16                                       TO WRK-COD-FUNC
AGO16                 MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
AGO16                 MOVE 'VIA PROGRAMA GFCT6045'
AGO16                                   TO GFCTE1-DESC-FUNC-INCL
AGO16                 MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
AGO16                 MOVE WRK-ANO               TO  WRK-ANO-DB2
AGO16                 MOVE WRK-MES               TO  WRK-MES-DB2
AGO16                 MOVE WRK-DIA               TO  WRK-DIA-DB2
AGO16                 MOVE WRK-HORA              TO  WRK-HORA-X
AGO16                 MOVE WRK-MIN               TO  WRK-MINU-X
AGO16                 MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
AGO16                 MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

SET15             WHEN 6046
SET15                 MOVE 'MODULO PRE ABERTURA - INCLUSAO CELULAR'
SET15                                   TO GFCTE1-CANAL-INCLUSAO
SET15                 MOVE CFUNC-MANUT-INCL OF GFCTB009
SET15                                       TO WRK-COD-FUNC
SET15                 MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
SET15                 MOVE 'VIA PROGRAMA GFCT6046'
SET15                                   TO GFCTE1-DESC-FUNC-INCL
SET15                 MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
SET15                 MOVE WRK-ANO               TO  WRK-ANO-DB2
SET15                 MOVE WRK-MES               TO  WRK-MES-DB2
SET15                 MOVE WRK-DIA               TO  WRK-DIA-DB2
SET15                 MOVE WRK-HORA              TO  WRK-HORA-X
SET15                 MOVE WRK-MIN               TO  WRK-MINU-X
SET15                 MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
SET15                 MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

JUN10             WHEN 6051
JUN10                 MOVE 'MODULO CORPORATIVO - INCLUSAO CELULAR'
JUN10                                   TO GFCTE1-CANAL-INCLUSAO
JUN10                 MOVE CFUNC-MANUT-INCL OF GFCTB009
JUN10                                       TO WRK-COD-FUNC
JUN10                 MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
JUN10                 MOVE 'VIA PROGRAMA GFCT6051'
JUN10                                   TO GFCTE1-DESC-FUNC-INCL
JUN10                 MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
JUN10                 MOVE WRK-ANO               TO  WRK-ANO-DB2
JUN10                 MOVE WRK-MES               TO  WRK-MES-DB2
JUN10                 MOVE WRK-DIA               TO  WRK-DIA-DB2
JUN10                 MOVE WRK-HORA              TO  WRK-HORA-X
JUN10                 MOVE WRK-MIN               TO  WRK-MINU-X
JUN10                 MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
JUN10                 MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

AGO16             WHEN 6400
AGO16                 MOVE 'BRADESCO NEXT - INCLUSAO'
AGO16                                   TO GFCTE1-CANAL-INCLUSAO
AGO16                 MOVE CFUNC-MANUT-INCL OF GFCTB009
AGO16                                       TO WRK-COD-FUNC
AGO16                 MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
AGO16                 MOVE 'VIA PROGRAMA GFCT6400'
AGO16                                   TO GFCTE1-DESC-FUNC-INCL
AGO16                 MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
AGO16                 MOVE WRK-ANO               TO  WRK-ANO-DB2
AGO16                 MOVE WRK-MES               TO  WRK-MES-DB2
AGO16                 MOVE WRK-DIA               TO  WRK-DIA-DB2
AGO16                 MOVE WRK-HORA              TO  WRK-HORA-X
AGO16                 MOVE WRK-MIN               TO  WRK-MINU-X
AGO16                 MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
AGO16                 MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

RJOVA             WHEN 6401
                      MOVE 'BRADESCO NEXT - TROCA PLANO'
                                        TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PROGRAMA CMTF3TNA'
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
RJOVA                 MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

RJOVA             WHEN 6898
                      MOVE 'MODULO MANUTENCAO - ATM'
                                        TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PROGRAMA GFCT6898'
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
RJOVA                 MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

RJOVA             WHEN 575
                      MOVE 'MODULO MANUTENCAO - MOBILE'
                                        TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PROGRAMA GFCT0575'
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
RJOVA                 MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL
RJOVA
                  WHEN 579
                      MOVE 'MODULO ABERTURA  - MOBILE'
                                        TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PROGRAMA GFCT0579'
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
RJOVA                 MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL


                  WHEN 578
                      MOVE 'MODULO ABERTURA  - MOBILE'
                                        TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PROGRAMA GFCT0578'
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                      MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

                  WHEN 6800
                      MOVE 'MODULO PREABERTURA P.A.- MANUT'
                                        TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PROGRAMA GFCT06800'
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
RJOVA                 MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

                  WHEN 7500
                      MOVE '- PROC. MANUT EXPRESSO PRIOR.'
                                        TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PROGRAMA GFCT07500'
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                      MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL


                  WHEN 6500
                      MOVE 'MANUTENCAO DE CONTAS '
                                        TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PROGRAMA '
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                      MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

                  WHEN 9070
                      MOVE 'BATCH - CONTAS NOVAS - A PEDIDO'
                                        TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PROGRAMA GFCT9070'
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                      MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

                  WHEN 6988
                      MOVE 'MODULO MANUTECAO  - CANAL ATM. '

                                        TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PROGRAMA GFCT6988'
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                      MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL


                  WHEN 12
                      MOVE 'BATCH - CONTAS NOVAS - A PEDIDO'
                                        TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PROGRAMA GFCT9070'
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                      MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

                  WHEN 9074
                      MOVE 'BATCH - CONTAS NOVAS TOMBAMENTO A PEDIDO'
                                        TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PROGRAMA GFCT9074'
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                      MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

                  WHEN 18
                      MOVE 'BATCH - CONTAS DE PAB - POSTAL-EXPRESSO'
                                        TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PROGRAMA GFCT9076'
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                      MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

                  WHEN 9076
                      MOVE 'BATCH - CONTAS DE PAB - POSTAL-EXPRESSO'
                                        TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PROGRAMA GFCT9076'
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                      MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

                  WHEN 10
                      MOVE 'BATCH - CONTAS NOVAS TOMBAMENTO A PEDIDO'
                                        TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PROGRAMA GFCT9074'
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                      MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

                  WHEN 9570
                      MOVE 'BATCH - CONTAS REJEITADAS - A PEDIDO'
                                        TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PROGRAMA GFCT9570'
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                      MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

                  WHEN 9913
                      MOVE 'BATCH - CANCELAMENTO PACOTES DUPLICADOS'
                                        TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PROGRAMA GFCT9913'
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                      MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

                  WHEN 2412
                      MOVE 'BATCH - EXCLUSAO MASSIVA - PAB'
                                     TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PGM GFCT2412'
                                            TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG        OF GFCTB009
                                            TO  WRK-TIMESTAMP
                      MOVE WRK-ANO          TO  WRK-ANO-DB2
                      MOVE WRK-MES          TO  WRK-MES-DB2
                      MOVE WRK-DIA          TO  WRK-DIA-DB2
                      MOVE WRK-HORA         TO  WRK-HORA-X
                      MOVE WRK-MIN          TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2     TO  GFCTE1-DATA-INCL
                      MOVE WRK-TIME-X       TO  GFCTE1-HORA-INCL

                  WHEN 2413
                       MOVE 'BATCH - EXCLUSAO MASSIVA - POSTAL'
                                        TO GFCTE1-CANAL-INCLUSAO
                       MOVE CFUNC-MANUT-INCL OF GFCTB009
                                             TO WRK-COD-FUNC
                       MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                       MOVE 'VIA PGM GFCT2413'
                                         TO GFCTE1-DESC-FUNC-INCL
                       MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                       MOVE WRK-ANO               TO  WRK-ANO-DB2
                       MOVE WRK-MES               TO  WRK-MES-DB2
                       MOVE WRK-DIA               TO  WRK-DIA-DB2
                       MOVE WRK-HORA              TO  WRK-HORA-X
                       MOVE WRK-MIN               TO  WRK-MINU-X
                       MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                       MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

                  WHEN 2414
                       MOVE 'BATCH - EXCLUSAO MASSIVA - EXPRESSO'
                                        TO GFCTE1-CANAL-INCLUSAO
                       MOVE CFUNC-MANUT-INCL OF GFCTB009
                                             TO WRK-COD-FUNC
                       MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                       MOVE 'VIA PGM GFCT2414'
                                         TO GFCTE1-DESC-FUNC-INCL
                       MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                       MOVE WRK-ANO               TO  WRK-ANO-DB2
                       MOVE WRK-MES               TO  WRK-MES-DB2
                       MOVE WRK-DIA               TO  WRK-DIA-DB2
                       MOVE WRK-HORA              TO  WRK-HORA-X
                       MOVE WRK-MIN               TO  WRK-MINU-X
                       MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                       MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

                  WHEN 2415
                       MOVE 'BATCH - EXCLUSAO MASSIVA - FORNECEDOR'
                                        TO GFCTE1-CANAL-INCLUSAO
                       MOVE CFUNC-MANUT-INCL OF GFCTB009
                                             TO WRK-COD-FUNC
                       MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                       MOVE 'VIA PGM GFCT2415'
                                         TO GFCTE1-DESC-FUNC-INCL
                       MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                       MOVE WRK-ANO               TO  WRK-ANO-DB2
                       MOVE WRK-MES               TO  WRK-MES-DB2
                       MOVE WRK-DIA               TO  WRK-DIA-DB2
                       MOVE WRK-HORA              TO  WRK-HORA-X
                       MOVE WRK-MIN               TO  WRK-MINU-X
                       MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                       MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

                  WHEN 2416
                       MOVE 'BATCH - EXCLUSAO MASSIVA - EMPRESA'
                                        TO GFCTE1-CANAL-INCLUSAO
                       MOVE CFUNC-MANUT-INCL OF GFCTB009
                                             TO WRK-COD-FUNC
                       MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                       MOVE 'VIA PGM GFCT2416'
                                         TO GFCTE1-DESC-FUNC-INCL
                       MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                       MOVE WRK-ANO               TO  WRK-ANO-DB2
                       MOVE WRK-MES               TO  WRK-MES-DB2
                       MOVE WRK-DIA               TO  WRK-DIA-DB2
                       MOVE WRK-HORA              TO  WRK-HORA-X
                       MOVE WRK-MIN               TO  WRK-MINU-X
                       MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                       MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

                  WHEN 1372
                       MOVE 'BATCH - CANCELAMENTO - CLICK CONTA'
                                        TO GFCTE1-CANAL-INCLUSAO
                       MOVE CFUNC-MANUT-INCL OF GFCTB009
                                             TO WRK-COD-FUNC
                       MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                       MOVE 'VIA PGM GFCT1372'
                                         TO GFCTE1-DESC-FUNC-INCL
                       MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                       MOVE WRK-ANO               TO  WRK-ANO-DB2
                       MOVE WRK-MES               TO  WRK-MES-DB2
                       MOVE WRK-DIA               TO  WRK-DIA-DB2
                       MOVE WRK-HORA              TO  WRK-HORA-X
                       MOVE WRK-MIN               TO  WRK-MINU-X
                       MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                       MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

JUN10             WHEN 5264
JUN10                  MOVE 'MODULO CORRESP NAO BANCARIO - INCLUSAO'
JUN10                                   TO GFCTE1-CANAL-INCLUSAO
JUN10                  MOVE CFUNC-MANUT-INCL OF GFCTB009
JUN10                                        TO WRK-COD-FUNC
JUN10                  MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
JUN10                  MOVE 'VIA PGM GFCT5264'
JUN10                                    TO GFCTE1-DESC-FUNC-INCL
JUN10                  MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
JUN10                  MOVE WRK-ANO               TO  WRK-ANO-DB2
JUN10                  MOVE WRK-MES               TO  WRK-MES-DB2
JUN10                  MOVE WRK-DIA               TO  WRK-DIA-DB2
JUN10                  MOVE WRK-HORA              TO  WRK-HORA-X
JUN10                  MOVE WRK-MIN               TO  WRK-MINU-X
JUN10                  MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
JUN10                  MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL
JUN10
JUN10             WHEN 5265
JUN10                  MOVE 'MODULO CORRESP N BANCARIO -ANULACAO INCL'
JUN10                                   TO GFCTE1-CANAL-INCLUSAO
JUN10                  MOVE CFUNC-MANUT-INCL OF GFCTB009
JUN10                                        TO WRK-COD-FUNC
JUN10                  MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
JUN10                  MOVE 'VIA PGM GFCT5265'
JUN10                                    TO GFCTE1-DESC-FUNC-INCL
JUN10                  MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
JUN10                  MOVE WRK-ANO               TO  WRK-ANO-DB2
JUN10                  MOVE WRK-MES               TO  WRK-MES-DB2
JUN10                  MOVE WRK-DIA               TO  WRK-DIA-DB2
JUN10                  MOVE WRK-HORA              TO  WRK-HORA-X
JUN10                  MOVE WRK-MIN               TO  WRK-MINU-X
JUN10                  MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
JUN10                  MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL
JUN10
JUN10             WHEN 5266
JUN10                  MOVE 'MODULO CORRESP NAO BANCARIO - ALTERACAO'
JUN10                                   TO GFCTE1-CANAL-INCLUSAO
JUN10                  MOVE CFUNC-MANUT-INCL OF GFCTB009
JUN10                                        TO WRK-COD-FUNC
JUN10                  MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
JUN10                  MOVE 'VIA PGM GFCT5266'
JUN10                                    TO GFCTE1-DESC-FUNC-INCL
JUN10                  MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
JUN10                  MOVE WRK-ANO               TO  WRK-ANO-DB2
JUN10                  MOVE WRK-MES               TO  WRK-MES-DB2
JUN10                  MOVE WRK-DIA               TO  WRK-DIA-DB2
JUN10                  MOVE WRK-HORA              TO  WRK-HORA-X
JUN10                  MOVE WRK-MIN               TO  WRK-MINU-X
JUN10                  MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
JUN10                  MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL
JUN10
JUN10             WHEN 5267
JUN10                  MOVE 'MODULO CORRESP N BANCARIO -ANULACAO ALT'
JUN10                                   TO GFCTE1-CANAL-INCLUSAO
JUN10                  MOVE CFUNC-MANUT-INCL OF GFCTB009
JUN10                                        TO WRK-COD-FUNC
JUN10                  MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
JUN10                  MOVE 'VIA PGM GFCT5267'
JUN10                                    TO GFCTE1-DESC-FUNC-INCL
JUN10                  MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
JUN10                  MOVE WRK-ANO               TO  WRK-ANO-DB2
JUN10                  MOVE WRK-MES               TO  WRK-MES-DB2
JUN10                  MOVE WRK-DIA               TO  WRK-DIA-DB2
JUN10                  MOVE WRK-HORA              TO  WRK-HORA-X
JUN10                  MOVE WRK-MIN               TO  WRK-MINU-X
JUN10                  MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
JUN10                  MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL
JUN10
JUN10             WHEN 5268
JUN10                  MOVE 'MODULO CORRESP NAO BANCARIO - CANCEL'
JUN10                                   TO GFCTE1-CANAL-INCLUSAO
JUN10                  MOVE CFUNC-MANUT-INCL OF GFCTB009
JUN10                                        TO WRK-COD-FUNC
JUN10                  MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
JUN10                  MOVE 'VIA PGM GFCT5268'
JUN10                                    TO GFCTE1-DESC-FUNC-INCL
JUN10                  MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
JUN10                  MOVE WRK-ANO               TO  WRK-ANO-DB2
JUN10                  MOVE WRK-MES               TO  WRK-MES-DB2
JUN10                  MOVE WRK-DIA               TO  WRK-DIA-DB2
JUN10                  MOVE WRK-HORA              TO  WRK-HORA-X
JUN10                  MOVE WRK-MIN               TO  WRK-MINU-X
JUN10                  MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
JUN10                  MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL
JUN10
JUN10             WHEN 5269
JUN10                  MOVE 'MODULO CORRESP N BANCARIO -ANULACAO CANC'
JUN10                                   TO GFCTE1-CANAL-INCLUSAO
JUN10                  MOVE CFUNC-MANUT-INCL OF GFCTB009
JUN10                                        TO WRK-COD-FUNC
JUN10                  MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
JUN10                  MOVE 'VIA PGM GFCT5269'
JUN10                                    TO GFCTE1-DESC-FUNC-INCL
JUN10                  MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
JUN10                  MOVE WRK-ANO               TO  WRK-ANO-DB2
JUN10                  MOVE WRK-MES               TO  WRK-MES-DB2
JUN10                  MOVE WRK-DIA               TO  WRK-DIA-DB2
JUN10                  MOVE WRK-HORA              TO  WRK-HORA-X
JUN10                  MOVE WRK-MIN               TO  WRK-MINU-X
JUN10                  MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
JUN10                  MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL
JUN10
MAI11             WHEN 2453
MAI11                  MOVE 'BATCH - CANCEL CESTA ELETRONICA - MENSAL'
MAI11                                   TO GFCTE1-CANAL-INCLUSAO
MAI11                  MOVE CFUNC-MANUT-INCL OF GFCTB009
MAI11                                        TO WRK-COD-FUNC
MAI11                  MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
MAI11                  MOVE 'VIA PGM GFCT2453'
MAI11                                    TO GFCTE1-DESC-FUNC-INCL
MAI11                  MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
MAI11                  MOVE WRK-ANO               TO  WRK-ANO-DB2
MAI11                  MOVE WRK-MES               TO  WRK-MES-DB2
MAI11                  MOVE WRK-DIA               TO  WRK-DIA-DB2
MAI11                  MOVE WRK-HORA              TO  WRK-HORA-X
MAI11                  MOVE WRK-MIN               TO  WRK-MINU-X
MAI11                  MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
MAI11                  MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL
MAI11
BI0412            WHEN 2182
BI0412                 MOVE '                                        '
BI0412                                  TO GFCTE1-CANAL-INCLUSAO
BI0412                 MOVE CFUNC-MANUT-INCL OF GFCTB009
BI0412                                       TO WRK-COD-FUNC
BI0412                 MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
BI1112                 MOVE 'ADESAO AUTOMATICA'
BI0412                                   TO GFCTE1-DESC-FUNC-INCL
BI0412                 MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
BI0412                 MOVE WRK-ANO               TO  WRK-ANO-DB2
BI0412                 MOVE WRK-MES               TO  WRK-MES-DB2
BI0412                 MOVE WRK-DIA               TO  WRK-DIA-DB2
BI0412                 MOVE WRK-HORA              TO  WRK-HORA-X
BI0412                 MOVE WRK-MIN               TO  WRK-MINU-X
BI0412                 MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
BI0412                 MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

BI0412            WHEN 6101
BI0412                 MOVE 'ADESAO INTERNET BANKING                 '
BI0412                                  TO GFCTE1-CANAL-INCLUSAO
BI0412                 MOVE CFUNC-MANUT-INCL OF GFCTB009
BI0412                                       TO WRK-COD-FUNC
BI0412                 MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
BI0412                 MOVE 'VIA PGM GFCT6101'
BI0412                                   TO GFCTE1-DESC-FUNC-INCL
BI0412                 MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
BI0412                 MOVE WRK-ANO               TO  WRK-ANO-DB2
BI0412                 MOVE WRK-MES               TO  WRK-MES-DB2
BI0412                 MOVE WRK-DIA               TO  WRK-DIA-DB2
BI0412                 MOVE WRK-HORA              TO  WRK-HORA-X
BI0412                 MOVE WRK-MIN               TO  WRK-MINU-X
BI0412                 MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
BI0412                 MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

BI0412            WHEN 6131
BI0412                 MOVE 'CANCELAMENTO INTERNET BANKING           '
BI0412                                  TO GFCTE1-CANAL-INCLUSAO
BI0412                 MOVE CFUNC-MANUT-INCL OF GFCTB009
BI0412                                       TO WRK-COD-FUNC
BI0412                 MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
BI0412                 MOVE 'VIA PGM GFCT6131'
BI0412                                   TO GFCTE1-DESC-FUNC-INCL
BI0412                 MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
BI0412                 MOVE WRK-ANO               TO  WRK-ANO-DB2
BI0412                 MOVE WRK-MES               TO  WRK-MES-DB2
BI0412                 MOVE WRK-DIA               TO  WRK-DIA-DB2
BI0412                 MOVE WRK-HORA              TO  WRK-HORA-X
BI0412                 MOVE WRK-MIN               TO  WRK-MINU-X
BI0412                 MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
BI0412                 MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

                  WHEN 3622
                       MOVE 'CARGA MASSIVA DE CESTAS E FLEXIBILIZACAO'
                                        TO GFCTE1-CANAL-INCLUSAO
                       MOVE CFUNC-MANUT-INCL OF GFCTB009
                                             TO WRK-COD-FUNC
                       MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                       MOVE 'VIA PGM GFCT3622'
                                         TO GFCTE1-DESC-FUNC-INCL
                       MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                       MOVE WRK-ANO               TO  WRK-ANO-DB2
                       MOVE WRK-MES               TO  WRK-MES-DB2
                       MOVE WRK-DIA               TO  WRK-DIA-DB2
                       MOVE WRK-HORA              TO  WRK-HORA-X
                       MOVE WRK-MIN               TO  WRK-MINU-X
                       MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                       MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

                  WHEN 6111
                       MOVE 'ADESAO CESTA CELULAR IB'
                                        TO GFCTE1-CANAL-INCLUSAO
                       MOVE CFUNC-MANUT-INCL OF GFCTB009
                                             TO WRK-COD-FUNC
                       MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                       MOVE 'VIA PGM GFCT6111'
                                         TO GFCTE1-DESC-FUNC-INCL
                       MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                       MOVE WRK-ANO               TO  WRK-ANO-DB2
                       MOVE WRK-MES               TO  WRK-MES-DB2
                       MOVE WRK-DIA               TO  WRK-DIA-DB2
                       MOVE WRK-HORA              TO  WRK-HORA-X
                       MOVE WRK-MIN               TO  WRK-MINU-X
                       MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                       MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

                  WHEN 6047
                       MOVE 'MODULO PREABERTURA P.A.- INCLUSAO'
                                        TO GFCTE1-CANAL-INCLUSAO
                       MOVE CFUNC-MANUT-INCL OF GFCTB009
                                             TO WRK-COD-FUNC
                       MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                       MOVE 'VIA PROGRAMA GFCT6047    '
                                         TO GFCTE1-DESC-FUNC-INCL
                       MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                       MOVE WRK-ANO               TO  WRK-ANO-DB2
                       MOVE WRK-MES               TO  WRK-MES-DB2
                       MOVE WRK-DIA               TO  WRK-DIA-DB2
                       MOVE WRK-HORA              TO  WRK-HORA-X
                       MOVE WRK-MIN               TO  WRK-MINU-X
                       MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                       MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL


                  WHEN 7800
                       MOVE 'MODULO ABERTURA EXPRESS - INCL'
                                        TO GFCTE1-CANAL-INCLUSAO
                       MOVE CFUNC-MANUT-INCL OF GFCTB009
                                             TO WRK-COD-FUNC
                       MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                       MOVE 'VIA PROGRAMA GFCT6047    '
                                         TO GFCTE1-DESC-FUNC-INCL
                       MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                       MOVE WRK-ANO               TO  WRK-ANO-DB2
                       MOVE WRK-MES               TO  WRK-MES-DB2
                       MOVE WRK-DIA               TO  WRK-DIA-DB2
                       MOVE WRK-HORA              TO  WRK-HORA-X
                       MOVE WRK-MIN               TO  WRK-MINU-X
                       MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                       MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

                  WHEN 9999206
                       MOVE 'ADESAO MASSIVA - CMTF'
                                        TO GFCTE1-CANAL-INCLUSAO
                       MOVE CFUNC-MANUT-INCL OF GFCTB009
                                             TO WRK-COD-FUNC
                       MOVE WRK-COD-FUNC-X7 TO GFCTE1-COD-FUNC-INCL
                       MOVE 'VIA PGM CMTF206F'
                                         TO GFCTE1-DESC-FUNC-INCL
                       MOVE HINCL-REG OF GFCTB009 TO WRK-TIMESTAMP
                       MOVE WRK-ANO              TO  WRK-ANO-DB2
                       MOVE WRK-MES              TO  WRK-MES-DB2
                       MOVE WRK-DIA              TO  WRK-DIA-DB2
                       MOVE WRK-HORA             TO  WRK-HORA-X
                       MOVE WRK-MIN              TO  WRK-MINU-X
                       MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-INCL
                       MOVE WRK-TIME-X           TO  GFCTE1-HORA-INCL


RJOVA             WHEN 7988
                      MOVE 'MODULO MANUNTENCAO - ATM'
                                        TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
                      MOVE 'VIA PROGRAMA GFT7988'
                                        TO GFCTE1-DESC-FUNC-INCL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
RJOVA                 MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL


                  WHEN OTHER
                      MOVE 'ON-LINE MANUAL' TO GFCTE1-CANAL-INCLUSAO
                      MOVE CFUNC-MANUT-INCL OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL

                      IF WRK-COD-FUNC-X7 NOT EQUAL ZEROS
                         PERFORM 2500-ACESSAR-SARH8000
                         MOVE WRK-DESC-FUNCIONARIO
                                            TO GFCTE1-DESC-FUNC-INCL
                      ELSE
                          MOVE SPACES       TO GFCTE1-DESC-FUNC-INCL
                      END-IF

                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                      MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL

           END-EVALUATE.

           IF HMANUT-REG OF GFCTB009 NOT EQUAL
                                    '0001-01-01-01.01.01.000001'

              EVALUATE CFUNC-MANUT      OF GFCTB009

                  WHEN 1
                      MOVE 'INDETERMINADO'  TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PGM GFCT1570 OU GFCT6011 OU GFCT9570'
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO                TO  WRK-ANO-DB2
                      MOVE WRK-MES                TO  WRK-MES-DB2
                      MOVE WRK-DIA                TO  WRK-DIA-DB2
                      MOVE WRK-HORA               TO  WRK-HORA-X
                      MOVE WRK-MIN                TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2           TO  GFCTE1-DATA-CANCEL
                      MOVE WRK-TIME-X             TO  GFCTE1-HORA-CANCEL

                  WHEN 1570
                      MOVE 'BATCH - CESTA CONTA CORRENTE - A PEDIDO'
                                        TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA GFCT1570'
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO                TO  WRK-ANO-DB2
                      MOVE WRK-MES                TO  WRK-MES-DB2
                      MOVE WRK-DIA                TO  WRK-DIA-DB2
                      MOVE WRK-HORA               TO  WRK-HORA-X
                      MOVE WRK-MIN                TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2           TO  GFCTE1-DATA-CANCEL
                      MOVE WRK-TIME-X             TO  GFCTE1-HORA-CANCEL

                  WHEN 1585
                      MOVE 'BATCH - CONTAS NOVAS - MENSAL'
                                        TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA GFCT1585'
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO                TO  WRK-ANO-DB2
                      MOVE WRK-MES                TO  WRK-MES-DB2
                      MOVE WRK-DIA                TO  WRK-DIA-DB2
                      MOVE WRK-HORA               TO  WRK-HORA-X
                      MOVE WRK-MIN                TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2           TO  GFCTE1-DATA-CANCEL
                      MOVE WRK-TIME-X             TO  GFCTE1-HORA-CANCEL

                  WHEN 15
                      MOVE 'BATCH - CONTAS NOVAS - MENSAL'
                                        TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA GFCT1585'
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO                TO  WRK-ANO-DB2
                      MOVE WRK-MES                TO  WRK-MES-DB2
                      MOVE WRK-DIA                TO  WRK-DIA-DB2
                      MOVE WRK-HORA               TO  WRK-HORA-X
                      MOVE WRK-MIN                TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2           TO  GFCTE1-DATA-CANCEL
                      MOVE WRK-TIME-X             TO  GFCTE1-HORA-CANCEL

                  WHEN 1910
                      MOVE 'BATCH - TRAG'
                                        TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA GFCT1910'
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO              TO  WRK-ANO-DB2
                      MOVE WRK-MES              TO  WRK-MES-DB2
                      MOVE WRK-DIA              TO  WRK-DIA-DB2
                      MOVE WRK-HORA             TO  WRK-HORA-X
                      MOVE WRK-MIN              TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
                      MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

220125            WHEN 2211
.                     MOVE 'BATCH - PRE TRAG'
.                                       TO GFCTE1-CANAL-INCLUSAO
.                     MOVE CFUNC-MANUT-INCL OF GFCTB009
.                                           TO WRK-COD-FUNC
.                     MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-INCL
.                     MOVE 'VIA PROGRAMA GFCT2B1A'
.                                       TO GFCTE1-DESC-FUNC-INCL
.                     MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
.                     MOVE WRK-ANO               TO  WRK-ANO-DB2
.                     MOVE WRK-MES               TO  WRK-MES-DB2
.                     MOVE WRK-DIA               TO  WRK-DIA-DB2
.                     MOVE WRK-HORA              TO  WRK-HORA-X
.                     MOVE WRK-MIN               TO  WRK-MINU-X
.                     MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
.                     MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL
220125
                  WHEN 1947
                      MOVE 'BATCH - CONTAS NOVAS - DIARIA'
                                        TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA GFCT1947'
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO              TO  WRK-ANO-DB2
                      MOVE WRK-MES              TO  WRK-MES-DB2
                      MOVE WRK-DIA              TO  WRK-DIA-DB2
                      MOVE WRK-HORA             TO  WRK-HORA-X
                      MOVE WRK-MIN              TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
                      MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

                  WHEN 14
                      MOVE 'BATCH - CONTAS NOVAS - DIARIA'
                                        TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA GFCT1947'
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO              TO  WRK-ANO-DB2
                      MOVE WRK-MES              TO  WRK-MES-DB2
                      MOVE WRK-DIA              TO  WRK-DIA-DB2
                      MOVE WRK-HORA             TO  WRK-HORA-X
                      MOVE WRK-MIN              TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
                      MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

                  WHEN 2108
                      MOVE 'BATCH - CONTAS CANCELADAS-MENSAL'
                                        TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA GFCT2108'
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO              TO  WRK-ANO-DB2
                      MOVE WRK-MES              TO  WRK-MES-DB2
                      MOVE WRK-DIA              TO  WRK-DIA-DB2
                      MOVE WRK-HORA             TO  WRK-HORA-X
                      MOVE WRK-MIN              TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
                      MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

                  WHEN 20
                      MOVE 'BATCH - CONTAS CANCELADAS-MENSAL'
                                        TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA GFCT2108'
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO              TO  WRK-ANO-DB2
                      MOVE WRK-MES              TO  WRK-MES-DB2
                      MOVE WRK-DIA              TO  WRK-DIA-DB2
                      MOVE WRK-HORA             TO  WRK-HORA-X
                      MOVE WRK-MIN              TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
                      MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

                  WHEN 2109
                      MOVE 'BATCH - CANCELAMENTO MASSIVO - A PEDIDO'
                                        TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA GFCT2109'
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO              TO  WRK-ANO-DB2
                      MOVE WRK-MES              TO  WRK-MES-DB2
                      MOVE WRK-DIA              TO  WRK-DIA-DB2
                      MOVE WRK-HORA             TO  WRK-HORA-X
                      MOVE WRK-MIN              TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
                      MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

                  WHEN 25
                      MOVE 'BATCH - CANCELAMENTO MASSIVO - A PEDIDO'
                                        TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA GFCT2109'
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO              TO  WRK-ANO-DB2
                      MOVE WRK-MES              TO  WRK-MES-DB2
                      MOVE WRK-DIA              TO  WRK-DIA-DB2
                      MOVE WRK-HORA             TO  WRK-HORA-X
                      MOVE WRK-MIN              TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
                      MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

                  WHEN 6011
                      MOVE 'MODULO CORPORATIVO - INCLUSAO'
                                        TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA GFCT6011'
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO              TO  WRK-ANO-DB2
                      MOVE WRK-MES              TO  WRK-MES-DB2
                      MOVE WRK-DIA              TO  WRK-DIA-DB2
                      MOVE WRK-HORA             TO  WRK-HORA-X
                      MOVE WRK-MIN              TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
                      MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

                  WHEN 6016
                      MOVE 'MODULO CORPORATIVO - CANCELAMENTO'
                                        TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA GFCT6016'
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO              TO  WRK-ANO-DB2
                      MOVE WRK-MES              TO  WRK-MES-DB2
                      MOVE WRK-DIA              TO  WRK-DIA-DB2
                      MOVE WRK-HORA             TO  WRK-HORA-X
                      MOVE WRK-MIN              TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
                      MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

                  WHEN 9999999
                      MOVE 'MODULO CORPORATIVO - CANCELAMENTO'
                                        TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA GFCT6016'
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO              TO  WRK-ANO-DB2
                      MOVE WRK-MES              TO  WRK-MES-DB2
                      MOVE WRK-DIA              TO  WRK-DIA-DB2
                      MOVE WRK-HORA             TO  WRK-HORA-X
                      MOVE WRK-MIN              TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
                      MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

AGO16             WHEN 6045
AGO16                 MOVE 'MODULO PRE ABERTURA - ALTERACAO'
AGO16                                   TO GFCTE1-CANAL-CANCEL
AGO16                 MOVE CFUNC-MANUT      OF GFCTB009
AGO16                                       TO WRK-COD-FUNC
AGO16                 MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
AGO16                 MOVE 'VIA PROGRAMA GFCT6045'
AGO16                                   TO GFCTE1-DESC-FUNC-CANCEL
AGO16                 MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
AGO16                 MOVE WRK-ANO               TO  WRK-ANO-DB2
AGO16                 MOVE WRK-MES               TO  WRK-MES-DB2
AGO16                 MOVE WRK-DIA               TO  WRK-DIA-DB2
AGO16                 MOVE WRK-HORA              TO  WRK-HORA-X
AGO16                 MOVE WRK-MIN               TO  WRK-MINU-X
AGO16                 MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
AGO16                 MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

SET15             WHEN 6046
SET15                 MOVE 'MODULO PRE ABERTURA - ALTERACAO'
SET15                                   TO GFCTE1-CANAL-CANCEL
SET15                 MOVE CFUNC-MANUT      OF GFCTB009
SET15                                       TO WRK-COD-FUNC
SET15                 MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
SET15                 MOVE 'VIA PROGRAMA GFCT6046'
SET15                                   TO GFCTE1-DESC-FUNC-CANCEL
SET15                 MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
SET15                 MOVE WRK-ANO               TO  WRK-ANO-DB2
SET15                 MOVE WRK-MES               TO  WRK-MES-DB2
SET15                 MOVE WRK-DIA               TO  WRK-DIA-DB2
SET15                 MOVE WRK-HORA              TO  WRK-HORA-X
SET15                 MOVE WRK-MIN               TO  WRK-MINU-X
SET15                 MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
SET15                 MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

JUN10             WHEN 6051
JUN10                 MOVE 'MODULO CORPORATIVO - INCLUSAO'
JUN10                                   TO GFCTE1-CANAL-CANCEL
JUN10                 MOVE CFUNC-MANUT      OF GFCTB009
JUN10                                       TO WRK-COD-FUNC
JUN10                 MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
JUN10                 MOVE 'VIA PROGRAMA GFCT6051'
JUN10                                   TO GFCTE1-DESC-FUNC-CANCEL
JUN10                 MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
JUN10                 MOVE WRK-ANO              TO  WRK-ANO-DB2
JUN10                 MOVE WRK-MES              TO  WRK-MES-DB2
JUN10                 MOVE WRK-DIA              TO  WRK-DIA-DB2
JUN10                 MOVE WRK-HORA             TO  WRK-HORA-X
JUN10                 MOVE WRK-MIN              TO  WRK-MINU-X
JUN10                 MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
JUN10                 MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

AGO16             WHEN 6400
AGO16                 MOVE 'BRADESCO NEXT - ALTERACAO'
AGO16                                   TO GFCTE1-CANAL-CANCEL
AGO16                 MOVE CFUNC-MANUT      OF GFCTB009
AGO16                                       TO WRK-COD-FUNC
AGO16                 MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
AGO16                 MOVE 'VIA PROGRAMA GFCT6400'
AGO16                                   TO GFCTE1-DESC-FUNC-CANCEL
AGO16                 MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
AGO16                 MOVE WRK-ANO               TO  WRK-ANO-DB2
AGO16                 MOVE WRK-MES               TO  WRK-MES-DB2
AGO16                 MOVE WRK-DIA               TO  WRK-DIA-DB2
AGO16                 MOVE WRK-HORA              TO  WRK-HORA-X
AGO16                 MOVE WRK-MIN               TO  WRK-MINU-X
AGO16                 MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
AGO16                 MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

RJOVA             WHEN 6401
                      MOVE 'BRADESCO NEXT - TROCA PLANO'
                                        TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA CMTF3TNA'
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
RJOVA                 MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

RJOVA             WHEN 6898
                      MOVE 'MODULO MANUTENCAO - ATM'
                                        TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA GFCT6898'
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
RJOVA                 MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

RJOVA             WHEN 575
                      MOVE 'MODULO MANUTENCAO - MOBILE'
                                        TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA GFCT0575'
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
RJOVA                 MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

RJOVA             WHEN 579
                      MOVE 'MODULO ABERTURA  - MOBILE'
                                        TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA GFCT0579'
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
RJOVA                 MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

                  WHEN 578
                      MOVE 'MODULO ABERTURA  - MOBILE'
                                        TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA GFCT0578'
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
                      MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

                  WHEN 6800
                      MOVE 'MODULO PREABERTURA P.A.- MANUT'
                                        TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA GFCT6800'
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
                      MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

                  WHEN 6047
                      MOVE 'MODULO PREABERTURA P.A.- INCLUSAO'
                                        TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA GFCT6047     '
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
                      MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

                  WHEN 6988
                      MOVE 'MODULO MANUTECAO  - CANAL ATM. '
                                        TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA GFCT6988     '
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
                      MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

                  WHEN 7500
                      MOVE '- PROC. MANUT EXPRESSO PRIOR.'
                                                TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT          OF GFCTB009
                                                TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7      TO
                                                GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA GFCT07500'
                                                TO
                                                GFCTE1-DESC-FUNC-CANCEL
                      MOVE HINCL-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-INCL
                      MOVE WRK-TIME-X            TO  GFCTE1-HORA-INCL


                  WHEN 6500
                      MOVE 'MANUTENCAO DE CONTAS '
                                        TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT  OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA '
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-CANCEL
                      MOVE WRK-TIME-X            TO  GFCTE1-HORA-CANCEL


                  WHEN 9070
                      MOVE 'BATCH - CONTAS NOVAS - A PEDIDO'
                                        TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA GFCT9070'
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO              TO  WRK-ANO-DB2
                      MOVE WRK-MES              TO  WRK-MES-DB2
                      MOVE WRK-DIA              TO  WRK-DIA-DB2
                      MOVE WRK-HORA             TO  WRK-HORA-X
                      MOVE WRK-MIN              TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
                      MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

                  WHEN 12
                      MOVE 'BATCH - CONTAS NOVAS - A PEDIDO'
                                        TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA GFCT9070'
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO              TO  WRK-ANO-DB2
                      MOVE WRK-MES              TO  WRK-MES-DB2
                      MOVE WRK-DIA              TO  WRK-DIA-DB2
                      MOVE WRK-HORA             TO  WRK-HORA-X
                      MOVE WRK-MIN              TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
                      MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

                  WHEN 9074
                      MOVE 'BATCH - CONTAS NOVAS TOMBAMENTO A PEDIDO'
                                        TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA GFCT9074'
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO              TO  WRK-ANO-DB2
                      MOVE WRK-MES              TO  WRK-MES-DB2
                      MOVE WRK-DIA              TO  WRK-DIA-DB2
                      MOVE WRK-HORA             TO  WRK-HORA-X
                      MOVE WRK-MIN              TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
                      MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

                  WHEN 10
                      MOVE 'BATCH - CONTAS NOVAS TOMBAMENTO A PEDIDO'
                                        TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA GFCT9074'
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO              TO  WRK-ANO-DB2
                      MOVE WRK-MES              TO  WRK-MES-DB2
                      MOVE WRK-DIA              TO  WRK-DIA-DB2
                      MOVE WRK-HORA             TO  WRK-HORA-X
                      MOVE WRK-MIN              TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
                      MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

                  WHEN 9076
                      MOVE 'BATCH - CONTAS DE PAB - POSTAL-EXPRESSO'
                                        TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA GFCT9076'
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO              TO  WRK-ANO-DB2
                      MOVE WRK-MES              TO  WRK-MES-DB2
                      MOVE WRK-DIA              TO  WRK-DIA-DB2
                      MOVE WRK-HORA             TO  WRK-HORA-X
                      MOVE WRK-MIN              TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
                      MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

                  WHEN 18
                      MOVE 'BATCH - CONTAS DE PAB - POSTAL-EXPRESSO'
                                        TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA GFCT9076'
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO              TO  WRK-ANO-DB2
                      MOVE WRK-MES              TO  WRK-MES-DB2
                      MOVE WRK-DIA              TO  WRK-DIA-DB2
                      MOVE WRK-HORA             TO  WRK-HORA-X
                      MOVE WRK-MIN              TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
                      MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

                  WHEN 9570
                      MOVE 'BATCH - CONTAS REJEITADAS - A PEDIDO'
                                        TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA GFCT9570'
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO              TO  WRK-ANO-DB2
                      MOVE WRK-MES              TO  WRK-MES-DB2
                      MOVE WRK-DIA              TO  WRK-DIA-DB2
                      MOVE WRK-HORA             TO  WRK-HORA-X
                      MOVE WRK-MIN              TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
                      MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

                  WHEN 9913
                      MOVE 'BATCH - CANCELAMENTO PACOTES DUPLICADOS'
                                        TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA GFCT9913'
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO              TO  WRK-ANO-DB2
                      MOVE WRK-MES              TO  WRK-MES-DB2
                      MOVE WRK-DIA              TO  WRK-DIA-DB2
                      MOVE WRK-HORA             TO  WRK-HORA-X
                      MOVE WRK-MIN              TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
                      MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

                  WHEN 2412
                        MOVE 'BATCH - EXCLUSAO MASSIVA - PAB'
                                              TO GFCTE1-CANAL-CANCEL
                        MOVE CFUNC-MANUT      OF GFCTB009
                                              TO WRK-COD-FUNC
                        MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                        MOVE 'VIA PGM GFCT2412'
                                          TO GFCTE1-DESC-FUNC-CANCEL
                        MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                        MOVE WRK-ANO          TO  WRK-ANO-DB2
                        MOVE WRK-MES          TO  WRK-MES-DB2
                        MOVE WRK-DIA          TO  WRK-DIA-DB2
                        MOVE WRK-HORA         TO  WRK-HORA-X
                        MOVE WRK-MIN          TO  WRK-MINU-X
                        MOVE WRK-DATA-DB2     TO  GFCTE1-DATA-CANCEL
                        MOVE WRK-TIME-X       TO  GFCTE1-HORA-CANCEL

                  WHEN 2413
                     MOVE 'BATCH - EXCLUSAO MASSIVA - POSTAL'
                                           TO GFCTE1-CANAL-CANCEL
                     MOVE CFUNC-MANUT      OF GFCTB009
                                           TO WRK-COD-FUNC
                     MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                     MOVE 'VIA PGM GFCT2413'
                                       TO GFCTE1-DESC-FUNC-CANCEL
                     MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                     MOVE WRK-ANO                TO  WRK-ANO-DB2
                     MOVE WRK-MES                TO  WRK-MES-DB2
                     MOVE WRK-DIA                TO  WRK-DIA-DB2
                     MOVE WRK-HORA               TO  WRK-HORA-X
                     MOVE WRK-MIN                TO  WRK-MINU-X
                     MOVE WRK-DATA-DB2           TO  GFCTE1-DATA-CANCEL
                     MOVE WRK-TIME-X             TO  GFCTE1-HORA-CANCEL

                  WHEN 2414
                     MOVE 'BATCH - EXCLUSAO MASSIVA - EXPRESSO'
                                           TO GFCTE1-CANAL-CANCEL
                     MOVE CFUNC-MANUT      OF GFCTB009
                                           TO WRK-COD-FUNC
                     MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                     MOVE 'VIA PGM GFCT2414'
                                       TO GFCTE1-DESC-FUNC-CANCEL
                     MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                     MOVE WRK-ANO                TO  WRK-ANO-DB2
                     MOVE WRK-MES                TO  WRK-MES-DB2
                     MOVE WRK-DIA                TO  WRK-DIA-DB2
                     MOVE WRK-HORA               TO  WRK-HORA-X
                     MOVE WRK-MIN                TO  WRK-MINU-X
                     MOVE WRK-DATA-DB2           TO  GFCTE1-DATA-CANCEL
                     MOVE WRK-TIME-X             TO  GFCTE1-HORA-CANCEL

                  WHEN 2415
                     MOVE 'BATCH - EXCLUSAO MASSIVA - FORNECEDOR'
                                           TO GFCTE1-CANAL-CANCEL
                     MOVE CFUNC-MANUT      OF GFCTB009
                                           TO WRK-COD-FUNC
                     MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                     MOVE 'VIA PGM GFCT2415'
                                       TO GFCTE1-DESC-FUNC-CANCEL
                     MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                     MOVE WRK-ANO                TO  WRK-ANO-DB2
                     MOVE WRK-MES                TO  WRK-MES-DB2
                     MOVE WRK-DIA                TO  WRK-DIA-DB2
                     MOVE WRK-HORA               TO  WRK-HORA-X
                     MOVE WRK-MIN                TO  WRK-MINU-X
                     MOVE WRK-DATA-DB2           TO  GFCTE1-DATA-CANCEL
                     MOVE WRK-TIME-X             TO  GFCTE1-HORA-CANCEL

                  WHEN 2416
                     MOVE 'BATCH - EXCLUSAO MASSIVA - EMPRESA'
                                           TO GFCTE1-CANAL-CANCEL
                     MOVE CFUNC-MANUT      OF GFCTB009
                                           TO WRK-COD-FUNC
                     MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                     MOVE 'VIA PGM GFCT2416'
                                       TO GFCTE1-DESC-FUNC-CANCEL
                     MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                     MOVE WRK-ANO                TO  WRK-ANO-DB2
                     MOVE WRK-MES                TO  WRK-MES-DB2
                     MOVE WRK-DIA                TO  WRK-DIA-DB2
                     MOVE WRK-HORA               TO  WRK-HORA-X
                     MOVE WRK-MIN                TO  WRK-MINU-X
                     MOVE WRK-DATA-DB2           TO  GFCTE1-DATA-CANCEL
                     MOVE WRK-TIME-X             TO  GFCTE1-HORA-CANCEL

                  WHEN 1372
                     MOVE 'BATCH - CANCELAMENTO - CLICK CONTA'
                                           TO GFCTE1-CANAL-CANCEL
                     MOVE CFUNC-MANUT      OF GFCTB009
                                           TO WRK-COD-FUNC
                     MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                     MOVE 'VIA PGM GFCT1372'
                                       TO GFCTE1-DESC-FUNC-CANCEL
                     MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                     MOVE WRK-ANO                TO  WRK-ANO-DB2
                     MOVE WRK-MES                TO  WRK-MES-DB2
                     MOVE WRK-DIA                TO  WRK-DIA-DB2
                     MOVE WRK-HORA               TO  WRK-HORA-X
                     MOVE WRK-MIN                TO  WRK-MINU-X
                     MOVE WRK-DATA-DB2           TO  GFCTE1-DATA-CANCEL
                     MOVE WRK-TIME-X             TO  GFCTE1-HORA-CANCEL

JUN10             WHEN 5264
JUN10                MOVE 'MODULO CORRESP NAO BANCARIO - INCLUSAO'
JUN10                                      TO GFCTE1-CANAL-CANCEL
JUN10                MOVE CFUNC-MANUT      OF GFCTB009
JUN10                                      TO WRK-COD-FUNC
JUN10                MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
JUN10                MOVE 'VIA PGM GFCT5264'
JUN10                                  TO GFCTE1-DESC-FUNC-CANCEL
JUN10                MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
JUN10                MOVE WRK-ANO                TO  WRK-ANO-DB2
JUN10                MOVE WRK-MES                TO  WRK-MES-DB2
JUN10                MOVE WRK-DIA                TO  WRK-DIA-DB2
JUN10                MOVE WRK-HORA               TO  WRK-HORA-X
JUN10                MOVE WRK-MIN                TO  WRK-MINU-X
JUN10                MOVE WRK-DATA-DB2           TO  GFCTE1-DATA-CANCEL
JUN10                MOVE WRK-TIME-X             TO  GFCTE1-HORA-CANCEL
JUN10
JUN10             WHEN 5265
JUN10                MOVE 'MODULO CORRESP N BANCARIO -ANULACAO INCL'
JUN10                                      TO GFCTE1-CANAL-CANCEL
JUN10                MOVE CFUNC-MANUT      OF GFCTB009
JUN10                                      TO WRK-COD-FUNC
JUN10                MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
JUN10                MOVE 'VIA PGM GFCT5265'
JUN10                                  TO GFCTE1-DESC-FUNC-CANCEL
JUN10                MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
JUN10                MOVE WRK-ANO                TO  WRK-ANO-DB2
JUN10                MOVE WRK-MES                TO  WRK-MES-DB2
JUN10                MOVE WRK-DIA                TO  WRK-DIA-DB2
JUN10                MOVE WRK-HORA               TO  WRK-HORA-X
JUN10                MOVE WRK-MIN                TO  WRK-MINU-X
JUN10                MOVE WRK-DATA-DB2           TO  GFCTE1-DATA-CANCEL
JUN10                MOVE WRK-TIME-X             TO  GFCTE1-HORA-CANCEL
JUN10
JUN10             WHEN 5266
JUN10                MOVE 'MODULO CORRESP NAO BANCARIO - ALTERACAO'
JUN10                                      TO GFCTE1-CANAL-CANCEL
JUN10                MOVE CFUNC-MANUT      OF GFCTB009
JUN10                                      TO WRK-COD-FUNC
JUN10                MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
JUN10                MOVE 'VIA PGM GFCT5266'
JUN10                                  TO GFCTE1-DESC-FUNC-CANCEL
JUN10                MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
JUN10                MOVE WRK-ANO                TO  WRK-ANO-DB2
JUN10                MOVE WRK-MES                TO  WRK-MES-DB2
JUN10                MOVE WRK-DIA                TO  WRK-DIA-DB2
JUN10                MOVE WRK-HORA               TO  WRK-HORA-X
JUN10                MOVE WRK-MIN                TO  WRK-MINU-X
JUN10                MOVE WRK-DATA-DB2           TO  GFCTE1-DATA-CANCEL
JUN10                MOVE WRK-TIME-X             TO  GFCTE1-HORA-CANCEL
JUN10
JUN10             WHEN 5267
JUN10                MOVE 'MODULO CORRESP N BANCARIO -ANULACAO ALT'
JUN10                                      TO GFCTE1-CANAL-CANCEL
JUN10                MOVE CFUNC-MANUT      OF GFCTB009
JUN10                                      TO WRK-COD-FUNC
JUN10                MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
JUN10                MOVE 'VIA PGM GFCT5267'
JUN10                                  TO GFCTE1-DESC-FUNC-CANCEL
JUN10                MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
JUN10                MOVE WRK-ANO                TO  WRK-ANO-DB2
JUN10                MOVE WRK-MES                TO  WRK-MES-DB2
JUN10                MOVE WRK-DIA                TO  WRK-DIA-DB2
JUN10                MOVE WRK-HORA               TO  WRK-HORA-X
JUN10                MOVE WRK-MIN                TO  WRK-MINU-X
JUN10                MOVE WRK-DATA-DB2           TO  GFCTE1-DATA-CANCEL
JUN10                MOVE WRK-TIME-X             TO  GFCTE1-HORA-CANCEL
JUN10
JUN10             WHEN 5268
JUN10                MOVE 'MODULO CORRESP NAO BANCARIO - CANCEL'
JUN10                                      TO GFCTE1-CANAL-CANCEL
JUN10                MOVE CFUNC-MANUT      OF GFCTB009
JUN10                                      TO WRK-COD-FUNC
JUN10                MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
JUN10                MOVE 'VIA PGM GFCT5268'
JUN10                                  TO GFCTE1-DESC-FUNC-CANCEL
JUN10                MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
JUN10                MOVE WRK-ANO                TO  WRK-ANO-DB2
JUN10                MOVE WRK-MES                TO  WRK-MES-DB2
JUN10                MOVE WRK-DIA                TO  WRK-DIA-DB2
JUN10                MOVE WRK-HORA               TO  WRK-HORA-X
JUN10                MOVE WRK-MIN                TO  WRK-MINU-X
JUN10                MOVE WRK-DATA-DB2           TO  GFCTE1-DATA-CANCEL
JUN10                MOVE WRK-TIME-X             TO  GFCTE1-HORA-CANCEL
JUN10
JUN10             WHEN 5269
JUN10                MOVE 'MODULO CORRESP N BANCARIO -ANULACAO CANC'
JUN10                                      TO GFCTE1-CANAL-CANCEL
JUN10                MOVE CFUNC-MANUT      OF GFCTB009
JUN10                                      TO WRK-COD-FUNC
JUN10                MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
JUN10                MOVE 'VIA PGM GFCT5269'
JUN10                                  TO GFCTE1-DESC-FUNC-CANCEL
JUN10                MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
JUN10                MOVE WRK-ANO                TO  WRK-ANO-DB2
JUN10                MOVE WRK-MES                TO  WRK-MES-DB2
JUN10                MOVE WRK-DIA                TO  WRK-DIA-DB2
JUN10                MOVE WRK-HORA               TO  WRK-HORA-X
JUN10                MOVE WRK-MIN                TO  WRK-MINU-X
JUN10                MOVE WRK-DATA-DB2           TO  GFCTE1-DATA-CANCEL
JUN10                MOVE WRK-TIME-X             TO  GFCTE1-HORA-CANCEL
MAI11
MAI11             WHEN 2453
VI0517               MOVE 'BATCH - CANCEL CESTA ELETRONICA - MENSAL'
VI0517                                TO GFCTE1-CANAL-CANCEL
MAI11                MOVE CFUNC-MANUT     OF GFCTB009
MAI11                                      TO WRK-COD-FUNC
MAI11                MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
MAI11                MOVE 'VIA PGM GFCT2453'
MAI11                                  TO GFCTE1-DESC-FUNC-CANCEL
MAI11                MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
MAI11                MOVE WRK-ANO               TO  WRK-ANO-DB2
MAI11                MOVE WRK-MES               TO  WRK-MES-DB2
MAI11                MOVE WRK-DIA               TO  WRK-DIA-DB2
MAI11                MOVE WRK-HORA              TO  WRK-HORA-X
MAI11                MOVE WRK-MIN               TO  WRK-MINU-X
MAI11                MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-CANCEL
MAI11                MOVE WRK-TIME-X            TO  GFCTE1-HORA-CANCEL

BI0412            WHEN 2182
BI0412               MOVE '                                        '
BI0412                                TO GFCTE1-CANAL-CANCEL
BI0412               MOVE CFUNC-MANUT     OF GFCTB009
BI0412                                     TO WRK-COD-FUNC
BI0412               MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
BI0412               MOVE 'VIA PGM GFCT2182'
BI0412                                 TO GFCTE1-DESC-FUNC-CANCEL
BI0412               MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
BI0412               MOVE WRK-ANO               TO  WRK-ANO-DB2
BI0412               MOVE WRK-MES               TO  WRK-MES-DB2
BI0412               MOVE WRK-DIA               TO  WRK-DIA-DB2
BI0412               MOVE WRK-HORA              TO  WRK-HORA-X
BI0412               MOVE WRK-MIN               TO  WRK-MINU-X
BI0412               MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-CANCEL
BI0412               MOVE WRK-TIME-X            TO  GFCTE1-HORA-CANCEL

BI0412            WHEN 6101
BI0412               MOVE 'ADESAO INTERNET BANKING                 '
BI0412                                TO GFCTE1-CANAL-CANCEL
BI0412               MOVE CFUNC-MANUT     OF GFCTB009
BI0412                                     TO WRK-COD-FUNC
BI0412               MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
BI0412               MOVE 'VIA PGM GFCT6101'
BI0412                                 TO GFCTE1-DESC-FUNC-CANCEL
BI0412               MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
BI0412               MOVE WRK-ANO               TO  WRK-ANO-DB2
BI0412               MOVE WRK-MES               TO  WRK-MES-DB2
BI0412               MOVE WRK-DIA               TO  WRK-DIA-DB2
BI0412               MOVE WRK-HORA              TO  WRK-HORA-X
BI0412               MOVE WRK-MIN               TO  WRK-MINU-X
BI0412               MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-CANCEL
BI0412               MOVE WRK-TIME-X            TO  GFCTE1-HORA-CANCEL

BI0412            WHEN 6131
BI0412               MOVE 'CANCELAMENTO INTERNET BANKING           '
BI0412                                TO GFCTE1-CANAL-CANCEL
BI0412               MOVE CFUNC-MANUT     OF GFCTB009
BI0412                                     TO WRK-COD-FUNC
BI0412               MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
BI0412               MOVE 'VIA PGM GFCT6131'
BI0412                                 TO GFCTE1-DESC-FUNC-CANCEL
BI0412               MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
BI0412               MOVE WRK-ANO               TO  WRK-ANO-DB2
BI0412               MOVE WRK-MES               TO  WRK-MES-DB2
BI0412               MOVE WRK-DIA               TO  WRK-DIA-DB2
BI0412               MOVE WRK-HORA              TO  WRK-HORA-X
BI0412               MOVE WRK-MIN               TO  WRK-MINU-X
BI0412               MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-CANCEL
BI0412               MOVE WRK-TIME-X            TO  GFCTE1-HORA-CANCEL

                  WHEN 3622
                     MOVE 'CARGA MASSIVA DE CESTAS E FLEXIBILIZACAO'
                                      TO GFCTE1-CANAL-CANCEL
                     MOVE CFUNC-MANUT     OF GFCTB009
                                           TO WRK-COD-FUNC
                     MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                     MOVE 'VIA PGM GFCT3622'
                                       TO GFCTE1-DESC-FUNC-CANCEL
                     MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                     MOVE WRK-ANO               TO  WRK-ANO-DB2
                     MOVE WRK-MES               TO  WRK-MES-DB2
                     MOVE WRK-DIA               TO  WRK-DIA-DB2
                     MOVE WRK-HORA              TO  WRK-HORA-X
                     MOVE WRK-MIN               TO  WRK-MINU-X
                     MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-CANCEL
                     MOVE WRK-TIME-X            TO  GFCTE1-HORA-CANCEL

                  WHEN 6111
                     MOVE 'ADESAO CESTA CELULAR IB'
                                      TO GFCTE1-CANAL-CANCEL
                     MOVE CFUNC-MANUT     OF GFCTB009
                                           TO WRK-COD-FUNC
                     MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                     MOVE 'VIA PGM GFCT6111'
                                       TO GFCTE1-DESC-FUNC-CANCEL
                     MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                     MOVE WRK-ANO               TO  WRK-ANO-DB2
                     MOVE WRK-MES               TO  WRK-MES-DB2
                     MOVE WRK-DIA               TO  WRK-DIA-DB2
                     MOVE WRK-HORA              TO  WRK-HORA-X
                     MOVE WRK-MIN               TO  WRK-MINU-X
                     MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-CANCEL
                     MOVE WRK-TIME-X            TO  GFCTE1-HORA-CANCEL

RJOVA             WHEN 7988
                      MOVE 'MODULO MANUNTENCAO - ATM'
                                        TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA GFCT7988'
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
RJOVA                 MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL


VI0517            WHEN 9999206
VI0517               MOVE 'ADESAO MASSIVA - CMTF'
VI0517                                TO GFCTE1-CANAL-CANCEL
VI0517               MOVE CFUNC-MANUT     OF GFCTB009
VI0517                                     TO WRK-COD-FUNC
VI0517               MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
VI0517               MOVE 'VIA PGM CMTF206F'
VI0517                                 TO GFCTE1-DESC-FUNC-CANCEL
VI0517               MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
VI0517               MOVE WRK-ANO               TO  WRK-ANO-DB2
VI0517               MOVE WRK-MES               TO  WRK-MES-DB2
VI0517               MOVE WRK-DIA               TO  WRK-DIA-DB2
VI0517               MOVE WRK-HORA              TO  WRK-HORA-X
VI0517               MOVE WRK-MIN               TO  WRK-MINU-X
VI0517               MOVE WRK-DATA-DB2          TO  GFCTE1-DATA-CANCEL
VI0517               MOVE WRK-TIME-X            TO  GFCTE1-HORA-CANCEL

EC01              WHEN 9999
                      MOVE 'ON-LINE MANUAL' TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL
                      MOVE 'VIA PROGRAMA GOTF3RAE'
                                        TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO               TO  WRK-ANO-DB2
                      MOVE WRK-MES               TO  WRK-MES-DB2
                      MOVE WRK-DIA               TO  WRK-DIA-DB2
                      MOVE WRK-HORA              TO  WRK-HORA-X
                      MOVE WRK-MIN               TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
EC01                  MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

                  WHEN OTHER
                      MOVE 'ON-LINE MANUAL' TO GFCTE1-CANAL-CANCEL
                      MOVE CFUNC-MANUT      OF GFCTB009
                                            TO WRK-COD-FUNC
                      MOVE WRK-COD-FUNC-X7  TO GFCTE1-COD-FUNC-CANCEL

                      IF WRK-COD-FUNC-X7 NOT EQUAL ZEROS
                         PERFORM 2500-ACESSAR-SARH8000
                         MOVE WRK-DESC-FUNCIONARIO
                                            TO GFCTE1-DESC-FUNC-CANCEL
                      ELSE
                          MOVE SPACES       TO GFCTE1-DESC-FUNC-CANCEL
                      END-IF

                      MOVE HMANUT-REG OF GFCTB009 TO  WRK-TIMESTAMP
                      MOVE WRK-ANO              TO  WRK-ANO-DB2
                      MOVE WRK-MES              TO  WRK-MES-DB2
                      MOVE WRK-DIA              TO  WRK-DIA-DB2
                      MOVE WRK-HORA             TO  WRK-HORA-X
                      MOVE WRK-MIN              TO  WRK-MINU-X
                      MOVE WRK-DATA-DB2         TO  GFCTE1-DATA-CANCEL
                      MOVE WRK-TIME-X           TO  GFCTE1-HORA-CANCEL

              END-EVALUATE
           ELSE
                      MOVE SPACES           TO GFCTE1-CANAL-CANCEL
                      MOVE ZEROS            TO GFCTE1-COD-FUNC-CANCEL
                      MOVE SPACES           TO GFCTE1-DESC-FUNC-CANCEL
                      MOVE SPACES           TO GFCTE1-DATA-CANCEL
                      MOVE SPACES           TO GFCTE1-HORA-CANCEL
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE ACESSO A TABELA GFCTB009                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-ACESSAR-GFCTB009           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTE0-AGENCIA    TO CJUNC-DEPDC        OF GFCTB009.
           MOVE GFCTE0-CONTA      TO CCTA-CLI           OF GFCTB009.
           MOVE GFCTE0-COD-TARIFA TO CSERVC-TARIF       OF GFCTB009.
           MOVE GFCTE0-HINCL-REG  TO HINCL-REG          OF GFCTB009.

           EXEC SQL
             SELECT
                   CJUNC_DEPDC,
                   CCTA_CLI,
                   CSERVC_TARIF,
                   HINCL_REG,
                   CFUNC_MANUT_INCL,
                   CFUNC_MANUT,
                   DINIC_ADSAO_INDVD,
                   DFIM_ADSAO_INDVD,
                   HMANUT_REG,
                   CINDCD_EXCL_REG
             INTO
                   :GFCTB009.CJUNC-DEPDC,
                   :GFCTB009.CCTA-CLI,
                   :GFCTB009.CSERVC-TARIF,
                   :GFCTB009.HINCL-REG,
                   :GFCTB009.CFUNC-MANUT-INCL,
                   :GFCTB009.CFUNC-MANUT,
                   :GFCTB009.DINIC-ADSAO-INDVD,
                   :GFCTB009.DFIM-ADSAO-INDVD,
                   :GFCTB009.HMANUT-REG,
                   :GFCTB009.CINDCD-EXCL-REG
             FROM   DB2PRD.ADSAO_INDVD_PCOTE
             WHERE
                  CJUNC_DEPDC        = :GFCTB009.CJUNC-DEPDC        AND
                  CCTA_CLI           = :GFCTB009.CCTA-CLI           AND
                  CSERVC_TARIF       = :GFCTB009.CSERVC-TARIF       AND
                  HINCL_REG          = :GFCTB009.HINCL-REG
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0               EQUAL 'W')
                MOVE '0010'            TO GFCTG2-COD-MSG
                PERFORM 1110-OBTER-DESC-MENSAGEM
                INITIALIZE             GFCT0M-ERRO-SQL
                MOVE ' - GFCTB009'     TO WRK-NOME-TAB
                MOVE 'GFCT0131'        TO GFCT0M-PROGRAMA
                MOVE 'DB2'             TO GFCT0M-TIPO-ACESSO
                MOVE 'ADSAO_INDVD_PCOTE'
                                       TO GFCT0M-NOME-TAB
                MOVE 'SELECT'          TO GFCT0M-COMANDO-SQL
                MOVE '0010'            TO GFCT0M-LOCAL
                MOVE SQLCA(1:136)      TO GFCT0M-SQLCA-AREA
                MOVE 9                 TO GFCTE1-ERRO
                MOVE SQLCODE           TO WRK-AUXILIAR-4
                MOVE WRK-AUX-4         TO GFCTE1-COD-SQL-ERRO
                MOVE SPACES            TO GFCTE1-DESC-MSG-ERRO
                MOVE 0010              TO GFCTE1-COD-MSG-ERRO
                PERFORM 2110-CONCATENAR-MSG
                PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTE1-ERRO
               MOVE ZEROS              TO GFCTE1-COD-SQL-ERRO
               MOVE 1455               TO GFCTE1-COD-MSG-ERRO
               MOVE '1455'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTE1-DESC-MSG-ERRO(1:70)
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CONCATENA MENSAGEM DE SAIDA.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-CONCATENAR-MSG             SECTION.
      *----------------------------------------------------------------*

           STRING GFCTG3-DESC-MSG      WRK-NOME-TAB
           DELIMITED BY '  '           INTO GFCTE1-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA OBTER A DESCRICAO DA TARIFA                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-OBTER-DESC-TARIFA          SECTION.
      *----------------------------------------------------------------*
           MOVE CSERVC-TARIF OF GFCTB009 TO CSERVC-TARIF OF GFCTB0D8.

           EXEC SQL
                SELECT
                   RSERVC_TARIF_REDZD
                INTO
                   :GFCTB0D8.RSERVC-TARIF-REDZD
                FROM   DB2PRD.SERVC_TARIF_PRINC
                WHERE
                       CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100)OR
               (SQLWARN0               EQUAL 'W')
                MOVE '0010'            TO GFCTG2-COD-MSG
                PERFORM 1110-OBTER-DESC-MENSAGEM
                INITIALIZE             GFCT0M-ERRO-SQL
                MOVE ' - GFCTB0D8'     TO WRK-NOME-TAB
                MOVE 'GFCT0131'        TO GFCT0M-PROGRAMA
                MOVE 'DB2'             TO GFCT0M-TIPO-ACESSO
                MOVE 'SERVC_TARIF_PRNC'
                                       TO GFCT0M-NOME-TAB
                MOVE 'SELECT'          TO GFCT0M-COMANDO-SQL
                MOVE '0020'            TO GFCT0M-LOCAL
                MOVE SQLCA(1:136)      TO GFCT0M-SQLCA-AREA
                MOVE 9                 TO GFCTE1-ERRO
                MOVE SQLCODE           TO WRK-AUXILIAR-4
                MOVE WRK-AUX-4         TO GFCTE1-COD-SQL-ERRO
                MOVE SPACES            TO GFCTE1-DESC-MSG-ERRO
                MOVE 0010              TO GFCTE1-COD-MSG-ERRO
                PERFORM 2110-CONCATENAR-MSG
                PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTE1-ERRO
               MOVE ZEROS              TO GFCTE1-COD-SQL-ERRO
               MOVE 1284               TO GFCTE1-COD-MSG-ERRO
               MOVE '1284'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTE1-DESC-MSG-ERRO(1:70)
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA OBTER A DESCRICAO DA AGENCIA                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-ACESSAR-GFCT5006           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT1V-ENTRADA.
           MOVE 680                    TO GFCT1V-LL.
           MOVE ZEROS                  TO GFCT1V-ZZ.
           MOVE GFCTE0-TRANSACAO       TO GFCT1V-TRANSACAO.
           MOVE GFCTE0-FUNCAO          TO GFCT1V-FUNCAO.
           MOVE 001                    TO GFCT1V-QTDE-OCOR.
           MOVE ZEROS                  TO GFCT1V-QTDE-TOT-REG.
           MOVE GFCTE0-FUNC-BDSCO      TO GFCT1V-FUNC-BDSCO.
           MOVE 'N'                    TO GFCT1V-FIM.
           MOVE GFCTE0-AGENCIA         TO GFCT1V-COD-DEPDC(1).
           MOVE SPACES                 TO GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE  GFCT1X-SAIDA
                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5006'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCT1V-ENTRADA
                                             GFCT1X-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTE1-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG05-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0131'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCT1X-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTE1-FIM
               MOVE GFCT1X-ERRO        TO GFCTE1-ERRO
               MOVE GFCT1X-COD-SQL-ERRO
                                       TO GFCTE1-COD-SQL-ERRO
               MOVE GFCT1X-COD-MSG-ERRO
                                       TO GFCTE1-COD-MSG-ERRO

               IF  GFCT1X-ERRO         EQUAL 1
                   MOVE GFCT1X-DESC-MSG-ERRO
                                       TO GFCTE1-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5006-' GFCT1X-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTE1-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA OBTER O CPJ / CNPJ                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-OBTER-DADOS-CLIENTE        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG0-ENTRADA.
           MOVE 100                    TO GFCTG0-LL.
           MOVE ZEROS                  TO GFCTG0-ZZ.
           MOVE GFCTE0-TRANSACAO       TO GFCTG0-TRANSACAO.
           MOVE GFCTE0-FUNCAO          TO GFCTG0-FUNCAO.
           MOVE GFCTE0-FUNC-BDSCO      TO GFCTG0-FUNC-BDSCO.
           MOVE GFCTE0-AGENCIA         TO GFCTG0-COD-DEPDC.
           MOVE GFCTE0-CONTA           TO GFCTG0-CCTA-CLI.
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
               MOVE 9                  TO GFCTE1-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG05-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0131'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

BI0216     IF  GFCTHA-COD-MSG-ERRO  EQUAL  1428
BI0216         GO     TO  2400-99-FIM
BI0216     END-IF.

           IF  GFCTHA-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTE1-FIM
               MOVE GFCTHA-ERRO        TO GFCTE1-ERRO
               MOVE GFCTHA-COD-SQL-ERRO
                                       TO GFCTE1-COD-SQL-ERRO
               MOVE GFCTHA-COD-MSG-ERRO
                                       TO GFCTE1-COD-MSG-ERRO

               IF  GFCTHA-ERRO         EQUAL 1
                   MOVE GFCTHA-DESC-MSG-ERRO
                                       TO GFCTE1-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5054-' GFCTHA-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTE1-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA OBTER O NOME DO FUNCIONARIO                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-ACESSAR-SARH8000           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-SAIDA.

           MOVE WRK-COD-FUNC           TO WRK-PESQUISA-ENT.
           MOVE 002                    TO WRK-CAMPO-ENT(1).
           MOVE 086                    TO WRK-TAMANHO-ENT.
           MOVE 'SARH8000'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-ENTRADA
                                             WRK-SAIDA
                                             WRK-ERRO-AREA
                                             WRK-SQLCA.

           IF  WRK-COD-RETORNO-SAI     EQUAL ZEROS
               MOVE ZEROS              TO GFCTE1-ERRO
                                          GFCTE1-COD-SQL-ERRO
                                          GFCTE1-COD-MSG-ERRO
               MOVE 'PROCESSAMENTO NORMAL'
                                       TO GFCTE1-DESC-MSG-ERRO
               MOVE WRK-REGISTRO-SAI(1:40)   TO WRK-DESC-FUNCIONARIO
           ELSE
               MOVE ZEROS              TO RETURN-CODE
               MOVE 'FUNCIONARIO NAO ENCONTRADO' TO WRK-DESC-FUNCIONARIO
           END-IF.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

1911  ******************************************************************
1911  *    ROTINA OBTER DADOS DAS OPERADORAS DE CELULAR                *
1911  ******************************************************************
1911  *----------------------------------------------------------------*
1911   2600-TRATAR-GFCTB0M2            SECTION.
1911  *----------------------------------------------------------------*
1911
1911       PERFORM 2610-ABRIR-CSR01
1911
1911       PERFORM 2620-LER-CSR01
1911
1911       PERFORM 2630-FECHAR-CSR01.
1911
1911  *----------------------------------------------------------------*
1911   2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

1911  ******************************************************************
1911  *ABRIR CURSOR DA TABELA GFCTB0M2                                 *
1911  ******************************************************************
1911  *----------------------------------------------------------------*
1911   2610-ABRIR-CSR01                SECTION.
1911  *----------------------------------------------------------------*
1911
1911       MOVE CJUNC-DEPDC            OF GFCTB009
1911                                   TO CAG-BCRIA        OF GFCTB0M2
1911       MOVE CCTA-CLI               OF GFCTB009
1911                                   TO CCTA-CLI         OF GFCTB0M2
1911       MOVE CSERVC-TARIF           OF GFCTB009
1911                                   TO CSERVC-TARIF     OF GFCTB0M2
1911       MOVE HINCL-REG              OF GFCTB009
1911                                   TO HINCL-REG        OF GFCTB0M2
1911
1911       EXEC SQL
1911           OPEN CSR01-GFCTB0M2
1911       END-EXEC.
1911
1911       IF  (SQLCODE                NOT EQUAL ZEROS )OR
1911           (SQLWARN0               EQUAL 'W')
1911            MOVE '0010'            TO GFCTG2-COD-MSG
1911            PERFORM 1110-OBTER-DESC-MENSAGEM
1911            INITIALIZE             GFCT0M-ERRO-SQL
1911            MOVE ' - GFCTB0M2'     TO WRK-NOME-TAB
1911            MOVE 'GFCT0131'        TO GFCT0M-PROGRAMA
1911            MOVE 'DB2'             TO GFCT0M-TIPO-ACESSO
1911            MOVE 'TADSAO_INDVD_CLULR'
1911                                   TO GFCT0M-NOME-TAB
1911            MOVE 'SELECT'          TO GFCT0M-COMANDO-SQL
1911            MOVE '0030'            TO GFCT0M-LOCAL
1911            MOVE SQLCA(1:136)      TO GFCT0M-SQLCA-AREA
1911            MOVE 9                 TO GFCTE1-ERRO
1911            MOVE SQLCODE           TO WRK-AUXILIAR-4
1911            MOVE WRK-AUX-4         TO GFCTE1-COD-SQL-ERRO
1911            MOVE SPACES            TO GFCTE1-DESC-MSG-ERRO
1911            MOVE 0010              TO GFCTE1-COD-MSG-ERRO
1911            PERFORM 2110-CONCATENAR-MSG
1911            PERFORM 3000-FINALIZAR
1911       END-IF.
1911
1911  *----------------------------------------------------------------*
1911   2610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

1911  ******************************************************************
1911  * LER CURSOR DA TABELA GFCTB0M2                                  *
1911  ******************************************************************
1911  *----------------------------------------------------------------*
1911   2620-LER-CSR01                  SECTION.
1911  *----------------------------------------------------------------*
1911
1911       EXEC SQL
1911         FETCH CSR01-GFCTB0M2   INTO
1911               :GFCTB0M2.NSEQ-REG-CLULR,
1911               :GFCTB0M2.COPER-FONE-MOVEL,
1911               :GFCTB0M2.CDDD-FONE-MOVEL,
022012             :GFCTB0M2.NLIN-TFONI
1911       END-EXEC.
1911
1911       IF  (SQLCODE                NOT EQUAL ZEROS AND +100)OR
1911           (SQLWARN0               EQUAL 'W')
1911            MOVE '0010'            TO GFCTG2-COD-MSG
1911            PERFORM 1110-OBTER-DESC-MENSAGEM
1911            INITIALIZE             GFCT0M-ERRO-SQL
1911            MOVE ' - GFCTB0M2'     TO WRK-NOME-TAB
1911            MOVE 'GFCT0131'        TO GFCT0M-PROGRAMA
1911            MOVE 'DB2'             TO GFCT0M-TIPO-ACESSO
1911            MOVE 'TADSAO_INDVD_CLULR'
1911                                   TO GFCT0M-NOME-TAB
1911            MOVE 'SELECT'          TO GFCT0M-COMANDO-SQL
1911            MOVE '0040'            TO GFCT0M-LOCAL
1911            MOVE SQLCA(1:136)      TO GFCT0M-SQLCA-AREA
1911            MOVE 9                 TO GFCTE1-ERRO
1911            MOVE SQLCODE           TO WRK-AUXILIAR-4
1911            MOVE WRK-AUX-4         TO GFCTE1-COD-SQL-ERRO
1911            MOVE SPACES            TO GFCTE1-DESC-MSG-ERRO
1911            MOVE 0010              TO GFCTE1-COD-MSG-ERRO
1911            PERFORM 2110-CONCATENAR-MSG
1911            PERFORM 3000-FINALIZAR
1911       END-IF.
1911
1911       IF  SQLCODE                 EQUAL +100
1911           MOVE ZEROS              TO GFCTE1-CD-OPERADORA
1911                                      GFCTE1-CD-DDD
1911                                      GFCTE1-NR-TELEFONE
1911           MOVE SPACES             TO GFCTE1-DS-OPERADORA
1911       ELSE
1911          MOVE COPER-FONE-MOVEL    OF GFCTB0M2
1911                                   TO WRK-AUX-9-S
1911          MOVE WRK-AUX-9           TO GFCTE1-CD-OPERADORA
1911          MOVE CDDD-FONE-MOVEL     OF GFCTB0M2
1911                                   TO WRK-AUX-3-S
1911          MOVE WRK-AUX-2           TO GFCTE1-CD-DDD
022012        MOVE NLIN-TFONI          OF GFCTB0M2
022012                                 TO WRK-AUX-11-S
022012        MOVE WRK-AUX-11          TO GFCTE1-NR-TELEFONE
MAR10         PERFORM 2621-OBTER-DESC-OPERAD
1911       END-IF.
1911
1911  *----------------------------------------------------------------*
1911   2620-99-FIM.                    EXIT.
1911  *----------------------------------------------------------------*

1911  ******************************************************************
1911  * ACESSAR DESCRICAO DA OPERADORA                                 *
1911  ******************************************************************
1911  *----------------------------------------------------------------*
1911   2621-OBTER-DESC-OPERAD          SECTION.
1911  *----------------------------------------------------------------*
1911
MAR10      MOVE COPER-FONE-MOVEL       OF GFCTB0M2
MAR10                                  TO COPER-FONE-MOVEL  OF GFCTB0M4
MAR10      MOVE CDDD-FONE-MOVEL        OF GFCTB0M2
MAR10                                  TO CDDD-FONE-MOVEL   OF GFCTB0M4
MAR10
MAR10      EXEC SQL
MAR10           SELECT
MAR10              ROPER_FONE_MOVEL
MAR10           INTO
MAR10              :GFCTB0M4.ROPER-FONE-MOVEL
MAR10           FROM
MAR10              DB2PRD.TOPER_FONE_CREDT
MAR10           WHERE
MAR10               COPER_FONE_MOVEL   = :GFCTB0M4.COPER-FONE-MOVEL
MAR10           AND CDDD_FONE_MOVEL    = :GFCTB0M4.CDDD-FONE-MOVEL
MAR10      END-EXEC.
MAR10
MAR10      IF  (SQLCODE                NOT EQUAL ZEROS AND +100)OR
MAR10          (SQLWARN0               EQUAL 'W')
MAR10           MOVE '0010'            TO GFCTG2-COD-MSG
MAR10           PERFORM 1110-OBTER-DESC-MENSAGEM
MAR10           INITIALIZE             GFCT0M-ERRO-SQL
MAR10           MOVE ' - GFCTB0M4'     TO WRK-NOME-TAB
MAR10           MOVE 'GFCT0131'        TO GFCT0M-PROGRAMA
MAR10           MOVE 'DB2'             TO GFCT0M-TIPO-ACESSO
MAR10           MOVE 'TOPER_FONE_CREDT'
MAR10                                  TO GFCT0M-NOME-TAB
MAR10           MOVE 'SELECT'          TO GFCT0M-COMANDO-SQL
MAR10           MOVE '0050'            TO GFCT0M-LOCAL
MAR10           MOVE SQLCA(1:136)      TO GFCT0M-SQLCA-AREA
MAR10           MOVE 9                 TO GFCTE1-ERRO
MAR10           MOVE SQLCODE           TO WRK-AUXILIAR-4
MAR10           MOVE WRK-AUX-4         TO GFCTE1-COD-SQL-ERRO
MAR10           MOVE SPACES            TO GFCTE1-DESC-MSG-ERRO
MAR10           MOVE 0010              TO GFCTE1-COD-MSG-ERRO
MAR10           PERFORM 2110-CONCATENAR-MSG
MAR10           PERFORM 3000-FINALIZAR
MAR10      END-IF.
MAR10
MAR10      IF  SQLCODE                 EQUAL +100
MAR10          MOVE SPACES             TO GFCTE1-DS-OPERADORA
MAR10      ELSE
MAR10          MOVE ROPER-FONE-MOVEL   OF GFCTB0M4
MAR10                                  TO GFCTE1-DS-OPERADORA
MAR10      END-IF.
MAR10
1911  *----------------------------------------------------------------*
1911   2621-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

1911  ******************************************************************
1911  *FECHAR CURSOR 1                                                 *
1911  ******************************************************************
1911  *----------------------------------------------------------------*
1911   2630-FECHAR-CSR01               SECTION.
1911  *----------------------------------------------------------------*
1911
1911       EXEC SQL
1911           CLOSE CSR01-GFCTB0M2
1911       END-EXEC.
1911
1911       IF  (SQLCODE                NOT EQUAL ZEROS )OR
1911           (SQLWARN0               EQUAL 'W')
1911            MOVE '0010'            TO GFCTG2-COD-MSG
1911            PERFORM 1110-OBTER-DESC-MENSAGEM
1911            INITIALIZE             GFCT0M-ERRO-SQL
1911            MOVE ' - GFCTB0M2'     TO WRK-NOME-TAB
1911            MOVE 'GFCT0131'        TO GFCT0M-PROGRAMA
1911            MOVE 'DB2'             TO GFCT0M-TIPO-ACESSO
1911            MOVE 'TADSAO_INDVD_CLULR'
1911                                   TO GFCT0M-NOME-TAB
1911            MOVE 'SELECT'          TO GFCT0M-COMANDO-SQL
1911            MOVE '0060'            TO GFCT0M-LOCAL
1911            MOVE SQLCA(1:136)      TO GFCT0M-SQLCA-AREA
1911            MOVE 9                 TO GFCTE1-ERRO
1911            MOVE SQLCODE           TO WRK-AUXILIAR-4
1911            MOVE WRK-AUX-4         TO GFCTE1-COD-SQL-ERRO
1911            MOVE SPACES            TO GFCTE1-DESC-MSG-ERRO
1911            MOVE 0010              TO GFCTE1-COD-MSG-ERRO
1911            PERFORM 2110-CONCATENAR-MSG
1911            PERFORM 3000-FINALIZAR
1911       END-IF.
1911
1911  *----------------------------------------------------------------*
1911   2630-99-FIM.                    EXIT.
1911  *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA FINALIZACAO DO PROCESSAMENTO.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 3100-RETORNAR-CHAMADOR.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA RETORNO AO CHAMADOR                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
