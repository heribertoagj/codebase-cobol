      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5121.
       AUTHOR.     ROBSON FAUSTO CESA.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT5121                                    *
      *    PROGRAMADOR.:   ROBSON FAUSTO CESA      - CPM PATO BRANCO   *
      *    ANALISTA CPM:   KHARUZO INOCENCIO LEITE - CPM PATO BRANCO   *
      *    ANALISTA....:   RICARDO PEREIRA         - PROCWORK /GP.50   *
      *    DATA........:   14/03/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR DADOS PARA COMBO DE TARIFAS - PERMIS- *
      *      SAO DE ADESAO.                                            *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                        INCLUDE/BOOK     *
      *                   DB2PRD.PRMSS_ADSAO_PCOTE      GFCTB0C6       *
      *                   DB2PRD.SERVC_TARIF_PRINC      GFCTB0D8       *
      *                   DB2PRD.PARM_DATA_PROCM        GFCTB0A1       *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTL3 - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCTL4 - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCTFZ - ENTRADA - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG1 - SAIDA   - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG2 - ENTRADA - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTG3 - SAIDA   - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5522 - VERIFICAR DISPONIBILIDADE DO ON-LINE.            *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    GFCT5507 - VERIFICA MASTER                                  *
      *    GFCT5516 - OBTER IDENTIFICACAO DO SOLICITANTE FLEXIBILIZACAO*
      *    SENH0315 - OBTER JUNCAO                                     *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                  *
      *    POOL1470 - CONSISTE DATA                                    *
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

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA INDEXADORES *'.
      *---------------------------------------------------------------*

       77  IND-1                       PIC  9(001)         VALUE ZEROS.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *---------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-NOM-TAB                 PIC  X(008)         VALUE SPACES.
       77  WRK-FINALIZAR               PIC  X(001)         VALUE SPACES.
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

       01  WRK-DATA-INV2               PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-INV2-R            REDEFINES WRK-DATA-INV2.
           05  WRK-ANO-INV2            PIC  9(004).
           05  WRK-MES-INV2            PIC  9(002).
           05  WRK-DIA-INV2            PIC  9(002).

       01  WRK-DATA-AUX.
           05  WRK-DIA-AUX             PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-MES-AUX             PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-ANO-AUX             PIC  9(004).

       01  WRK-DATA-PONTEIRO           PIC  X(010)         VALUE SPACES.

       01  WRK-MSG001.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA AO MODULO '.
           05 WRK-MODULO-MSG           PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA DE COMUNICACAO COM SENH0315 *'.
      *---------------------------------------------------------------*

       01  WRK-VERSAO-0315             PIC  X(006)         VALUE SPACES.
       01  WRK-MENSAGEM-0315.
           05  WRK-COD-MENSAGEM-0315   PIC  9(003)         VALUE ZEROS.
           05  WRK-HIFEN-MENSAGEM-0315 PIC  X(001)         VALUE SPACES.
           05  WRK-TEXTO-MENSAGEM-0315 PIC  X(079)         VALUE SPACES.
       01  WRK-COD-FUNCIONAL-0315      PIC  X(007)         VALUE SPACES.
       01  WRK-NOME-FUNC-0315          PIC  X(040)         VALUE SPACES.
       01  WRK-COD-BANCO-0315          PIC  9(005)         VALUE ZEROS.
       01  WRK-NOME-BANCO-0315         PIC  X(040)         VALUE SPACES.
       01  WRK-CNPJ-BANCO-0315.
ST25X6*     05  WRK-CNPJ-PRI-0315       PIC  9(009)         VALUE ZEROS.
ST25X6*     05  WRK-CNPJ-FIL-0315       PIC  9(005)         VALUE ZEROS.
ST25X6*     05  WRK-CNPJ-CON-0315       PIC  9(002)         VALUE ZEROS.
           05  WRK-CNPJ-PRI-0315       PIC  X(009)         VALUE SPACES.
           05  WRK-CNPJ-FIL-0315       PIC  X(004)         VALUE SPACES.
           05  WRK-CNPJ-CON-0315       PIC  X(002)         VALUE SPACES.
       01  WRK-COD-JUNCAO-0315         PIC  9(005)         VALUE ZEROS.
       01  WRK-NOME-DEPTO-0315         PIC  X(040)         VALUE SPACES.
       01  WRK-SECAO-0315              PIC  9(003)         VALUE ZEROS.
       01  WRK-STATUS-0315             PIC  X(001)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5507 *'.
      *---------------------------------------------------------------*

       01  WRK-5507-AREA.
           05  WRK-5507-AREA-ENVIA.
               10  WRK-5507-AMBIENTE         PIC  X(001)  VALUE SPACES.
               10  WRK-5507-CJUNC-DEPDC-X.
                   15  WRK-5507-CJUNC-DEPDC  PIC  9(005)  VALUE ZEROS.
           05  WRK-5507-AREA-RECEBE.
               10  WRK-5507-COD-RETORNO      PIC  9(002)  VALUE ZEROS.
               10  WRK-5507-COD-SQL-ERRO     PIC  9(003)  VALUE ZEROS.
               10  WRK-5507-MSG-RETORNO      PIC  X(045)  VALUE SPACES.
               10  WRK-5507-IDTFD-MASTER     PIC  X(001)  VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5522 - ENTRADA *'.
      *---------------------------------------------------------------*

       COPY I#GFCTFZ.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5522 - SAIDA *'.
      *---------------------------------------------------------------*

       COPY I#GFCTG1.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5523 - ENTRADA *'.
      *---------------------------------------------------------------*

       COPY I#GFCTG2.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5523 - SAIDA *'.
      *---------------------------------------------------------------*

       COPY I#GFCTG3.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TABELA DB2 *'.
      *---------------------------------------------------------------*

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
             INCLUDE GFCTB0A1
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DECLARACAO DE CURSORES *'.
      *---------------------------------------------------------------*

           EXEC SQL
             DECLARE   CSR01-GFCTB0C6   CURSOR   FOR
               SELECT DISTINCT
                   CSERVC_TARIF
             FROM  DB2PRD.PRMSS_ADSAO_PCOTE
             WHERE
                   CSERVC_TARIF      >= :GFCTB0C6.CSERVC-TARIF
             ORDER BY
                   CSERVC_TARIF
           END-EXEC.


           EXEC SQL
             DECLARE   CSR02-GFCTB0C6   CURSOR   FOR
               SELECT DISTINCT
                   CSERVC_TARIF
             FROM  DB2PRD.PRMSS_ADSAO_PCOTE
             WHERE
                   CSERVC_TARIF      >= :GFCTB0C6.CSERVC-TARIF       AND
                   DINIC_PRMSS_PCOTE <= :GFCTB0C6.DINIC-PRMSS-PCOTE  AND
                   DFIM_PRMSS_PCOTE  >= :GFCTB0C6.DFIM-PRMSS-PCOTE
             ORDER BY
                   CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE   CSR03-GFCTB0C6   CURSOR   FOR
               SELECT DISTINCT
                   CSERVC_TARIF
             FROM  DB2PRD.PRMSS_ADSAO_PCOTE
             WHERE
                   CSERVC_TARIF      >= :GFCTB0C6.CSERVC-TARIF       AND
                  (DINIC_PRMSS_PCOTE > :GFCTB0C6.DINIC-PRMSS-PCOTE   OR
                   DFIM_PRMSS_PCOTE  <  :GFCTB0C6.DFIM-PRMSS-PCOTE)
             ORDER BY
                   CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE   CSR04-GFCTB0C6   CURSOR   FOR
               SELECT DISTINCT
                   CSERVC_TARIF
             FROM  DB2PRD.PRMSS_ADSAO_PCOTE
             WHERE
                  (CSERVC_TARIF      >= :GFCTB0C6.CSERVC-TARIF)      AND
                 ((DINIC_PRMSS_PCOTE <= :GFCTB0C6.DINIC-PRMSS-PCOTE  AND
                  (DFIM_PRMSS_PCOTE  >= :GFCTB0C6.DFIM-PRMSS-PCOTE)  OR
                   DINIC_PRMSS_PCOTE >  :GFCTB0C6.DINIC-PRMSS-PCOTE))
             ORDER BY
                   CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE   CSR05-GFCTB0C6   CURSOR   FOR
               SELECT DISTINCT
                   CSERVC_TARIF
             FROM  DB2PRD.PRMSS_ADSAO_PCOTE
             WHERE
                   CSERVC_TARIF      >= :GFCTB0C6.CSERVC-TARIF      AND
                   DFIM_PRMSS_PCOTE  <  :GFCTB0C6.DFIM-PRMSS-PCOTE
             ORDER BY
                   CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE   CSR06-GFCTB0C6   CURSOR   FOR
               SELECT DISTINCT
                   CSERVC_TARIF
             FROM  DB2PRD.PRMSS_ADSAO_PCOTE
             WHERE
                   CSERVC_TARIF      >= :GFCTB0C6.CSERVC-TARIF      AND
                   DINIC_PRMSS_PCOTE > :GFCTB0C6.DINIC-PRMSS-PCOTE
             ORDER BY
                   CSERVC_TARIF
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY I#GFCTL3.

       COPY I#GFCTL4.

       COPY I#GFCT0M.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTL3-ENTRADA
                                                      GFCTL4-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *CHAMA AS ROTINAS PRINCIPAIS DO PROCESSAMENTO                    *
      ******************************************************************
      *----------------------------------------------------------------*
       00000-INICIAR                   SECTION.
      *----------------------------------------------------------------*

           PERFORM 10000-INICIALIZAR.

           PERFORM 20000-PROCESSAR.

           PERFORM 30000-FINALIZAR.

      *----------------------------------------------------------------*
       00000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCEDIMENTOS INICIAIS DO PROGRAMA                              *
      ******************************************************************
      *----------------------------------------------------------------*
       10000-INICIALIZAR               SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          GFCTL4-SAIDA.

           INITIALIZE GFCTL4-SAIDA
                      GFCT0M-AREA-ERROS
                      IND-1.

           PERFORM 11000-VERIFICAR-DISP-SISTEMA.

           PERFORM 12000-MONTAR-AREA-SAIDA.

           PERFORM 13000-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       10000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICA DISPONIBILIDADE DO SISTEMA                             *
      ******************************************************************
      *----------------------------------------------------------------*
       11000-VERIFICAR-DISP-SISTEMA    SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG1-SAIDA.

           INITIALIZE GFCTG1-SAIDA.

           MOVE +100                   TO GFCTFZ-LL.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE GFCTL3-TRANSACAO       TO GFCTFZ-TRANSACAO.
           MOVE GFCTL3-FUNCAO          TO GFCTFZ-FUNCAO.
           MOVE GFCTL3-FUNC-BDSCO      TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.

           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTL4-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5121'         TO GFCT0M-TRANSACAO
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 9
               MOVE 9                  TO GFCTL4-ERRO
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'GFCT5121'     TO GFCT0M-TRANSACAO
               END-IF
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE          EQUAL 1
               MOVE 9                      TO GFCTL4-ERRO
               MOVE 'SISTEMA INDISPONIVEL' TO GFCT0M-TEXTO
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5121'             TO GFCT0M-TRANSACAO
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       11000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *MONTA AREA DE SAIDA                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       12000-MONTAR-AREA-SAIDA         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE GFCTL4-SAIDA.

           MOVE 268                        TO GFCTL4-LL.
           MOVE ZEROS                      TO GFCTL4-ZZ.
           MOVE GFCTL3-TRANSACAO           TO GFCTL4-TRANSACAO.
           MOVE GFCTL3-FUNCAO              TO GFCTL4-FUNCAO.
           MOVE GFCTL3-FILTRO              TO GFCTL4-FILTRO.

           IF  GFCTL3-QTDE-OCOR            NOT NUMERIC
               MOVE ZEROS                  TO GFCTL4-QTDE-OCOR
           ELSE
               MOVE GFCTL3-QTDE-OCOR       TO GFCTL4-QTDE-OCOR
           END-IF.

           IF  GFCTL3-QTDE-TOT-REG         NOT NUMERIC
               MOVE ZEROS                  TO GFCTL4-QTDE-TOT-REG
           ELSE
               MOVE GFCTL3-QTDE-TOT-REG    TO GFCTL4-QTDE-TOT-REG
           END-IF.

           MOVE GFCTL3-FUNC-BDSCO          TO GFCTL4-FUNC-BDSCO.
           MOVE GFCTL3-PONTEIRO-TARIFA     TO GFCTL4-PONTEIRO-TARIFA.
           MOVE GFCTL3-PONTEIRO-DATA       TO GFCTL4-PONTEIRO-DATA.

           MOVE 'N'                        TO GFCTL4-FIM.

           MOVE ZEROS                      TO GFCTL4-ERRO
                                              GFCTL4-COD-SQL-ERRO
                                              GFCTL4-COD-MSG-ERRO.
           MOVE '0000'                     TO GFCTG2-COD-MSG.

           PERFORM 13100-OBTER-DESC-MSG.

           MOVE  GFCTG3-DESC-MSG           TO GFCTL4-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       12000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTE DADOS                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       13000-CONSISTIR-DADOS           SECTION.
      *----------------------------------------------------------------*

           IF (GFCTL3-TRANSACAO        EQUAL SPACES OR LOW-VALUES) OR
              (GFCTL3-FUNCAO           EQUAL SPACES OR LOW-VALUES) OR
              (GFCTL3-QTDE-OCOR        NOT NUMERIC) OR
              (GFCTL3-QTDE-TOT-REG     NOT NUMERIC) OR
              (GFCTL3-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES) OR
              (GFCTL3-PONTEIRO-TARIFA  NOT NUMERIC) OR
              (GFCTL3-TIPO-VIGENCIA    NOT EQUAL 'V' AND 'N' AND
                                                 'T' AND 'P' AND
                                                 'H' AND 'F')      OR
              (GFCTL3-TIPO-DATA        NOT EQUAL 0 AND 1 AND
                                                 2 AND 3)          OR
              (GFCTL3-TIPO-DATA        EQUAL 0 AND
               (GFCTL3-DATA-REFER      EQUAL SPACES OR LOW-VALUES))
               MOVE  1                 TO GFCTL4-ERRO
               MOVE  ZEROS             TO GFCTL4-COD-SQL-ERRO
               MOVE  0001              TO GFCTL4-COD-MSG-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTL4-FIM
               PERFORM 13100-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTL4-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       13000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTEM DESCRICAO DA MENSAGEM                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       13100-OBTER-DESC-MSG            SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE GFCTL3-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTL3-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTL3-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTL4-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5121'         TO GFCT0M-TRANSACAO
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 9                  TO GFCTL4-ERRO
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'GFCT5121'     TO GFCT0M-TRANSACAO
               END-IF
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       13100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAMENTO PRINCIPAL                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       20000-PROCESSAR                 SECTION.
      *----------------------------------------------------------------*

           PERFORM 21000-OBTER-COD-JUNCAO.

           PERFORM 22000-VERIFICA-MASTER.

           IF  GFCTL3-TIPO-DATA        EQUAL ZEROS
               MOVE GFCTL3-DATA-REFER  TO WRK-DATA-DB2
               INSPECT WRK-DATA-DB2 REPLACING ALL '/' BY '.'
               PERFORM 23000-CONSISTIR-DATA-WEB
               IF  WRK-ERRO-DATA       EQUAL 'S'
                   MOVE  1             TO GFCTL4-ERRO
                   MOVE  ZEROS         TO GFCTL4-COD-SQL-ERRO
                   MOVE  0125          TO GFCTL4-COD-MSG-ERRO
                   MOVE '0125'         TO GFCTG2-COD-MSG
                   MOVE 'S'            TO GFCTL4-FIM
                   PERFORM 13100-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTL4-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR
               END-IF
           ELSE
               PERFORM 24000-OBTER-DATA-PROCM
               IF  GFCTL3-TIPO-DATA    EQUAL 1
                   MOVE DPROCM-ANTER   OF GFCTB0A1
                                       TO WRK-DATA-DB2
               ELSE
                   IF  GFCTL3-TIPO-DATA    EQUAL 2
                       MOVE DPROCM-ATUAL   OF GFCTB0A1
                                       TO WRK-DATA-DB2
                   ELSE
                       MOVE DPROCM-PROX    OF GFCTB0A1
                                       TO WRK-DATA-DB2
                   END-IF
               END-IF
           END-IF.

           PERFORM 25000-TRATAR-CURSORES.

      *----------------------------------------------------------------*
       20000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA O MODULO SENH0315                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       21000-OBTER-COD-JUNCAO          SECTION.
      *----------------------------------------------------------------*

           MOVE 'VRS001'               TO WRK-VERSAO-0315.
           MOVE GFCTL3-FUNC-BDSCO      TO WRK-COD-FUNCIONAL-0315.

           CALL 'SENH0315'             USING WRK-VERSAO-0315
                                             WRK-MENSAGEM-0315
                                             WRK-COD-FUNCIONAL-0315
                                             WRK-NOME-FUNC-0315
                                             WRK-COD-BANCO-0315
                                             WRK-NOME-BANCO-0315
                                             WRK-CNPJ-BANCO-0315
                                             WRK-COD-JUNCAO-0315
                                             WRK-NOME-DEPTO-0315
                                             WRK-SECAO-0315
                                             WRK-STATUS-0315.

           IF  RETURN-CODE             NOT EQUAL ZEROS AND 04
               MOVE 1                  TO GFCTL4-ERRO
               MOVE RETURN-CODE        TO GFCTL4-COD-SQL-ERRO
               MOVE 0056               TO GFCTL4-COD-MSG-ERRO
               MOVE '0056'             TO GFCTL4-COD-MSG-ERRO
               PERFORM 13100-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTL4-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  RETURN-CODE             EQUAL 04
               IF  WRK-COD-MENSAGEM-0315   EQUAL 001
                   MOVE 1                  TO GFCTL4-ERRO
                   MOVE ZEROS              TO GFCTL4-COD-SQL-ERRO
                   MOVE 0057               TO GFCTL4-COD-MSG-ERRO
                   MOVE '0057'             TO GFCTG2-COD-MSG
                   PERFORM 13100-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTL4-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR
               END-IF
               IF  WRK-COD-MENSAGEM-0315   EQUAL 154
                   MOVE 1                  TO GFCTL4-ERRO
                   MOVE ZEROS              TO GFCTL4-COD-SQL-ERRO
                   MOVE 0058               TO GFCTL4-COD-MSG-ERRO
                   MOVE '0058'             TO GFCTG2-COD-MSG
                   PERFORM 13100-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTL4-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR
               END-IF
               IF  WRK-COD-MENSAGEM-0315   EQUAL 242
                   MOVE 1                  TO GFCTL4-ERRO
                   MOVE ZEROS              TO GFCTL4-COD-SQL-ERRO
                   MOVE 0059               TO GFCTL4-COD-MSG-ERRO
                   MOVE '0059'             TO GFCTG2-COD-MSG
                   PERFORM 13100-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTL4-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR
               END-IF
               IF  WRK-COD-MENSAGEM-0315   EQUAL 009
                   MOVE 1                  TO GFCTL4-ERRO
                   MOVE ZEROS              TO GFCTL4-COD-SQL-ERRO
                                              GFCTL4-COD-MSG-ERRO
                   MOVE '0000'             TO GFCTG2-COD-MSG
                   PERFORM 13100-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTL4-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR
               END-IF
               IF  WRK-COD-MENSAGEM-0315   EQUAL 138
                   MOVE 1                  TO GFCTL4-ERRO
                   MOVE ZEROS              TO GFCTL4-COD-SQL-ERRO
                   MOVE 0060               TO GFCTL4-COD-MSG-ERRO
                   MOVE '0060'             TO GFCTG2-COD-MSG
                   PERFORM 13100-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTL4-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       21000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA O MODULO GFCT5507                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       22000-VERIFICA-MASTER           SECTION.
      *----------------------------------------------------------------*

           MOVE 'O'                    TO WRK-5507-AMBIENTE.
           MOVE WRK-COD-JUNCAO-0315    TO WRK-5507-CJUNC-DEPDC.
           MOVE 'GFCT5507'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5507-AREA.

           IF  WRK-5507-COD-RETORNO    NOT EQUAL ZEROS
               IF  WRK-5507-COD-RETORNO    EQUAL 01
                   MOVE 1                  TO GFCTL4-ERRO
                   MOVE ZEROS              TO GFCTL4-COD-SQL-ERRO
                                              GFCTL4-COD-MSG-ERRO
                   MOVE WRK-5507-MSG-RETORNO
                                           TO GFCTL4-DESC-MSG-ERRO
               ELSE
                   IF  WRK-5507-COD-RETORNO    EQUAL 99
                       MOVE 1              TO GFCTL4-ERRO
                       MOVE WRK-5507-COD-SQL-ERRO TO GFCTL4-COD-SQL-ERRO
                       MOVE ZEROS          TO GFCTL4-COD-MSG-ERRO
                       MOVE WRK-5507-MSG-RETORNO
                                           TO GFCTL4-DESC-MSG-ERRO
                   END-IF
               END-IF
               INITIALIZE GFCTL4-DADOS
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       22000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTE DATA                                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       23000-CONSISTIR-DATA-WEB        SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-ERRO-DATA.

           IF  WRK-DATA-DB2            EQUAL SPACES OR LOW-VALUES
               MOVE 'S'                TO WRK-ERRO-DATA
           ELSE
               MOVE WRK-DIA-DB2        TO WRK-DIA-POOL1470
               MOVE WRK-MES-DB2        TO WRK-MES-POOL1470
               MOVE WRK-ANO-DB2        TO WRK-ANO-POOL1470
               PERFORM 23100-ACESSAR-POOL1470
           END-IF.

      *----------------------------------------------------------------*
       23000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTE DATA ATRAVES DA POOL1470                               *
      ******************************************************************
      *----------------------------------------------------------------*
       23100-ACESSAR-POOL1470          SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1470'             USING WRK-DATA-POOL1470.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'S'                TO WRK-ERRO-DATA
           END-IF.

      *----------------------------------------------------------------*
       23100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTEM DATA DO PROCESSAMENTO                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       24000-OBTER-DATA-PROCM          SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ANTER,
                   DPROCM_PROX,
                   DPROCM_ATUAL
             INTO
                   :GFCTB0A1.DPROCM-ANTER,
                   :GFCTB0A1.DPROCM-PROX,
                   :GFCTB0A1.DPROCM-ATUAL
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC        = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_DATA_PROCM ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTL4-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A1'         TO WRK-NOM-TAB
               PERFORM 24100-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       24000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FORMATA ERROS DB2                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       24100-FORMATAR-ERRO-DB2         SECTION.
      *----------------------------------------------------------------*

           MOVE  9                     TO GFCTL4-ERRO.
           MOVE 'GFCT5121'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE  SQLCA                 TO GFCT0M-SQLCA-AREA.
           MOVE  SQLCODE               TO GFCT0M-SQLCODE
                                          GFCTL4-COD-SQL-ERRO.

           PERFORM 13100-OBTER-DESC-MSG.

           STRING GFCTG3-DESC-MSG ' - ' WRK-NOM-TAB
           DELIMITED BY '  '           INTO GFCTL4-DESC-MSG-ERRO.

           INITIALIZE GFCTL4-DADOS.

           PERFORM 30000-FINALIZAR.

      *----------------------------------------------------------------*
       24100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *TRATAR CURSORES                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       25000-TRATAR-CURSORES           SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTL3-TIPO-VIGENCIA

               WHEN 'T'
                   PERFORM 25100-TRATAR-CSR01

               WHEN 'V'
                   PERFORM 25200-TRATAR-CSR02

               WHEN 'N'
                   PERFORM 25300-TRATAR-CSR03

               WHEN 'P'
                   PERFORM 25400-TRATAR-CSR04

               WHEN 'H'
                   PERFORM 25500-TRATAR-CSR05

               WHEN OTHER
                   PERFORM 25600-TRATAR-CSR06

           END-EVALUATE.

      *----------------------------------------------------------------*
       25000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *TRATAR CURSOR 1                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       25100-TRATAR-CSR01              SECTION.
      *----------------------------------------------------------------*

           PERFORM 25110-ABRIR-CSR01.

           PERFORM 25120-LER-CSR01.

           PERFORM 25130-PROCESSAR-CSR01   UNTIL
                   WRK-FINALIZAR           EQUAL 'S'.

           PERFORM 25140-FECHAR-CSR01.

      *----------------------------------------------------------------*
       25100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRE O CURSOR 1                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       25110-ABRIR-CSR01               SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTL3-PONTEIRO-TARIFA TO CSERVC-TARIF OF GFCTB0C6.
           MOVE 'N'                    TO WRK-FINALIZAR.

           EXEC SQL
               OPEN CSR01-GFCTB0C6
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PRMSS_ADSAO_PCOTE' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTL4-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0C6'         TO WRK-NOM-TAB
               PERFORM 24100-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       25110-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EFETUA LEITURA NO CURSOR 1                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       25120-LER-CSR01                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH   CSR01-GFCTB0C6   INTO
                   :GFCTB0C6.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PRMSS_ADSAO_PCOTE' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTL4-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0C6'         TO WRK-NOM-TAB
               PERFORM 24100-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTL4-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO GFCTL4-ERRO
                   MOVE ZEROS          TO GFCTL4-COD-SQL-ERRO
                   MOVE 0698           TO GFCTL4-COD-MSG-ERRO
                   MOVE '0698'         TO GFCTG2-COD-MSG
                   MOVE 'GFCTB0C6'     TO WRK-NOM-TAB
                   PERFORM 13100-OBTER-DESC-MSG
                   STRING GFCTG3-DESC-MSG ' - ' WRK-NOM-TAB
                   DELIMITED BY '  '           INTO GFCTL4-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       25120-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSA O CURSOR 1                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       25130-PROCESSAR-CSR01           SECTION.
      *----------------------------------------------------------------*

           PERFORM 25131-ACESSAR-GFCTB0D8.

           IF  WRK-5507-IDTFD-MASTER   EQUAL 'N'
               IF  CDEPDC              OF GFCTB0D8
                                       NOT EQUAL WRK-COD-JUNCAO-0315
                   PERFORM 25120-LER-CSR01
                   GO TO 25130-99-FIM
               END-IF
           END-IF.

           COMPUTE GFCTL4-LL           = GFCTL4-LL + 106.

           ADD 1                       TO GFCTL4-QTDE-TOT-REG
                                          IND-1.

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO GFCTL4-COD-TARIFA(IND-1).
           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO GFCTL4-DESC-TARIFA(IND-1).
           MOVE 'T'                    TO GFCTL4-TIPO-VIGENCIA(IND-1).

           PERFORM 25120-LER-CSR01.

           IF  WRK-FINALIZAR           EQUAL 'S'
               GO TO 25130-99-FIM
           END-IF.

           IF  IND-1                   GREATER 6
               MOVE 'S'                TO WRK-FINALIZAR
               MOVE CSERVC-TARIF       OF GFCTB0C6
                                       TO GFCTL4-PONTEIRO-TARIFA
           END-IF.

      *----------------------------------------------------------------*
       25130-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSAR TABELA GFCTB0D8                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       25131-ACESSAR-GFCTB0D8          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF
                                       OF GFCTB0D8.

           EXEC SQL
             SELECT
                   CDEPDC,
                   RSERVC_TARIF_REDZD
             INTO
                   :GFCTB0D8.CDEPDC,
                   :GFCTB0D8.RSERVC-TARIF-REDZD
             FROM   DB2PRD.SERVC_TARIF_PRINC
             WHERE
                   CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SERVC_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTL4-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0D8'         TO WRK-NOM-TAB
               PERFORM 24100-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       25131-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHA O CURSOR 1                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       25140-FECHAR-CSR01              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE   CSR01-GFCTB0C6
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SERVC_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTL4-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0C6'         TO WRK-NOM-TAB
               PERFORM 24100-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       25140-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *TRATAR CURSOR 2                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       25200-TRATAR-CSR02              SECTION.
      *----------------------------------------------------------------*

           PERFORM 25210-ABRIR-CSR02.

           PERFORM 25220-LER-CSR02.

           PERFORM 25230-PROCESSAR-CSR02   UNTIL
                   WRK-FINALIZAR           EQUAL 'S'.

           PERFORM 25240-FECHAR-CSR02.

      *----------------------------------------------------------------*
       25200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRE O CURSOR 2                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       25210-ABRIR-CSR02               SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTL3-PONTEIRO-TARIFA TO CSERVC-TARIF      OF GFCTB0C6.
           MOVE WRK-DATA-DB2           TO DINIC-PRMSS-PCOTE OF GFCTB0C6
                                          DFIM-PRMSS-PCOTE  OF GFCTB0C6.
           MOVE 'N'                    TO WRK-FINALIZAR.

           EXEC SQL
               OPEN CSR02-GFCTB0C6
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PRMSS_ADSAO_PCOTE' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTL4-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0C6'         TO WRK-NOM-TAB
               PERFORM 24100-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       25210-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EFETUA LEITURA NO CURSOR 2                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       25220-LER-CSR02                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH   CSR02-GFCTB0C6   INTO
                   :GFCTB0C6.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PRMSS_ADSAO_PCOTE' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTL4-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0C6'         TO WRK-NOM-TAB
               PERFORM 24100-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTL4-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO GFCTL4-ERRO
                   MOVE ZEROS          TO GFCTL4-COD-SQL-ERRO
                   MOVE 0699           TO GFCTL4-COD-MSG-ERRO
                   MOVE '0699'         TO GFCTG2-COD-MSG
                   MOVE 'GFCTB0C6'     TO WRK-NOM-TAB
                   PERFORM 13100-OBTER-DESC-MSG
                   STRING GFCTG3-DESC-MSG ' - ' WRK-NOM-TAB
                   DELIMITED BY '  '           INTO GFCTL4-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       25220-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSA O CURSOR 2                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       25230-PROCESSAR-CSR02           SECTION.
      *----------------------------------------------------------------*

           PERFORM 25131-ACESSAR-GFCTB0D8.

           IF  WRK-5507-IDTFD-MASTER   EQUAL 'N'
               IF  CDEPDC              OF GFCTB0D8
                                       NOT EQUAL WRK-COD-JUNCAO-0315
                   PERFORM 25220-LER-CSR02
                   GO TO 25230-99-FIM
               END-IF
           END-IF.

           COMPUTE GFCTL4-LL           = GFCTL4-LL + 106.

           ADD 1                       TO GFCTL4-QTDE-TOT-REG
                                          IND-1.

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO GFCTL4-COD-TARIFA(IND-1).
           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO GFCTL4-DESC-TARIFA(IND-1).
           MOVE 'V'                    TO GFCTL4-TIPO-VIGENCIA(IND-1).

           PERFORM 25220-LER-CSR02.

           IF  WRK-FINALIZAR           EQUAL 'S'
               GO TO 25230-99-FIM
           END-IF.

           IF  IND-1                   GREATER 6
               MOVE 'S'                TO WRK-FINALIZAR
               MOVE CSERVC-TARIF       OF GFCTB0C6
                                       TO GFCTL4-PONTEIRO-TARIFA
           END-IF.

      *----------------------------------------------------------------*
       25230-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHA O CURSOR 2                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       25240-FECHAR-CSR02              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE   CSR02-GFCTB0C6
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SERVC_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTL4-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0C6'         TO WRK-NOM-TAB
               PERFORM 24100-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       25240-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *TRATAR CURSOR 3                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       25300-TRATAR-CSR03              SECTION.
      *----------------------------------------------------------------*

           PERFORM 25310-ABRIR-CSR03.

           PERFORM 25320-LER-CSR03.


           PERFORM 25330-PROCESSAR-CSR03   UNTIL
                   WRK-FINALIZAR           EQUAL 'S'.

           PERFORM 25340-FECHAR-CSR03.

      *----------------------------------------------------------------*
       25300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRE O CURSOR 3                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       25310-ABRIR-CSR03               SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTL3-PONTEIRO-TARIFA TO CSERVC-TARIF      OF GFCTB0C6.
           MOVE WRK-DATA-DB2           TO DINIC-PRMSS-PCOTE OF GFCTB0C6
                                          DFIM-PRMSS-PCOTE  OF GFCTB0C6.
           MOVE 'N'                    TO WRK-FINALIZAR.

           EXEC SQL
               OPEN CSR03-GFCTB0C6
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PRMSS_ADSAO_PCOTE' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0090'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTL4-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0C6'         TO WRK-NOM-TAB
               PERFORM 24100-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       25310-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EFETUA LEITURA NO CURSOR 3                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       25320-LER-CSR03                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH   CSR03-GFCTB0C6   INTO
                   :GFCTB0C6.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PRMSS_ADSAO_PCOTE' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0100'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTL4-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0C6'         TO WRK-NOM-TAB
               PERFORM 24100-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTL4-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO GFCTL4-ERRO
                   MOVE ZEROS          TO GFCTL4-COD-SQL-ERRO
                   MOVE 0700           TO GFCTL4-COD-MSG-ERRO
                   MOVE '0700'         TO GFCTG2-COD-MSG
                   MOVE 'GFCTB0C6'     TO WRK-NOM-TAB
                   PERFORM 13100-OBTER-DESC-MSG
                   STRING GFCTG3-DESC-MSG ' - ' WRK-NOM-TAB
                   DELIMITED BY '  '           INTO GFCTL4-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       25320-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSA O CURSOR 3                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       25330-PROCESSAR-CSR03           SECTION.
      *----------------------------------------------------------------*

           PERFORM 25131-ACESSAR-GFCTB0D8.

           IF  WRK-5507-IDTFD-MASTER   EQUAL 'N'
               IF  CDEPDC              OF GFCTB0D8
                                       NOT EQUAL WRK-COD-JUNCAO-0315
                   PERFORM 25320-LER-CSR03
                   GO TO 25330-99-FIM
               END-IF
           END-IF.

           COMPUTE GFCTL4-LL           = GFCTL4-LL + 106.

           ADD 1                       TO GFCTL4-QTDE-TOT-REG
                                          IND-1.

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO GFCTL4-COD-TARIFA(IND-1).
           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO GFCTL4-DESC-TARIFA(IND-1).
           MOVE SPACES                 TO GFCTL4-TIPO-VIGENCIA(IND-1).

           PERFORM 25320-LER-CSR03.

           IF  WRK-FINALIZAR           EQUAL 'S'
               GO TO 25330-99-FIM
           END-IF.

           IF  IND-1                   GREATER 6
               MOVE 'S'                TO WRK-FINALIZAR
               MOVE CSERVC-TARIF       OF GFCTB0C6
                                       TO GFCTL4-PONTEIRO-TARIFA
           END-IF.

      *----------------------------------------------------------------*
       25330-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHA O CURSOR 3                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       25340-FECHAR-CSR03              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE   CSR03-GFCTB0C6
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SERVC_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0110'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTL4-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0C6'         TO WRK-NOM-TAB
               PERFORM 24100-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       25340-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *TRATAR CURSOR 4                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       25400-TRATAR-CSR04              SECTION.
      *----------------------------------------------------------------*

           PERFORM 25410-ABRIR-CSR04.

           PERFORM 25420-LER-CSR04.

           PERFORM 25430-PROCESSAR-CSR04   UNTIL
                   WRK-FINALIZAR           EQUAL 'S'.

           PERFORM 25440-FECHAR-CSR04.

      *----------------------------------------------------------------*
       25400-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRE O CURSOR 4                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       25410-ABRIR-CSR04               SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTL3-PONTEIRO-TARIFA TO CSERVC-TARIF      OF GFCTB0C6.

           MOVE WRK-DATA-DB2           TO DINIC-PRMSS-PCOTE OF GFCTB0C6
                                          DFIM-PRMSS-PCOTE  OF GFCTB0C6.

           MOVE 'N'                    TO WRK-FINALIZAR.

           EXEC SQL
               OPEN CSR04-GFCTB0C6
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PRMSS_ADSAO_PCOTE' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0120'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTL4-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0C6'         TO WRK-NOM-TAB
               PERFORM 24100-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       25410-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EFETUA LEITURA NO CURSOR 2                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       25420-LER-CSR04                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH   CSR04-GFCTB0C6   INTO
                   :GFCTB0C6.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PRMSS_ADSAO_PCOTE' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0130'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTL4-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0C6'         TO WRK-NOM-TAB
               PERFORM 24100-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTL4-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO GFCTL4-ERRO
                   MOVE ZEROS          TO GFCTL4-COD-SQL-ERRO
                   MOVE 0701           TO GFCTL4-COD-MSG-ERRO
                   MOVE '0701'         TO GFCTG2-COD-MSG
                   MOVE 'GFCTB0C6'     TO WRK-NOM-TAB
                   PERFORM 13100-OBTER-DESC-MSG
                   STRING GFCTG3-DESC-MSG ' - ' WRK-NOM-TAB
                   DELIMITED BY '  '           INTO GFCTL4-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       25420-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSA O CURSOR 4                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       25430-PROCESSAR-CSR04           SECTION.
      *----------------------------------------------------------------*

           PERFORM 25131-ACESSAR-GFCTB0D8.

           IF  WRK-5507-IDTFD-MASTER   EQUAL 'N'
               IF  CDEPDC              OF GFCTB0D8
                                       NOT EQUAL WRK-COD-JUNCAO-0315
                   PERFORM 25420-LER-CSR04
                   GO TO 25430-99-FIM
               END-IF
           END-IF.

           COMPUTE GFCTL4-LL           = GFCTL4-LL + 106.

           ADD 1                       TO GFCTL4-QTDE-TOT-REG
                                          IND-1.

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO GFCTL4-COD-TARIFA(IND-1).
           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO GFCTL4-DESC-TARIFA(IND-1).

           MOVE SPACES                 TO GFCTL4-TIPO-VIGENCIA(IND-1).

           PERFORM 25420-LER-CSR04.

           IF  WRK-FINALIZAR           EQUAL 'S'
               GO TO 25430-99-FIM
           END-IF.

           IF  IND-1                   GREATER 6
               MOVE 'S'                TO WRK-FINALIZAR
               MOVE CSERVC-TARIF       OF GFCTB0C6
                                       TO GFCTL4-PONTEIRO-TARIFA
           END-IF.

      *----------------------------------------------------------------*
       25430-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHA O CURSOR 4                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       25440-FECHAR-CSR04              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE   CSR04-GFCTB0C6
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SERVC_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0140'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTL4-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0C6'         TO WRK-NOM-TAB
               PERFORM 24100-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       25440-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *TRATAR CURSOR 5                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       25500-TRATAR-CSR05              SECTION.
      *----------------------------------------------------------------*

           PERFORM 25510-ABRIR-CSR05.

           PERFORM 25520-LER-CSR05.

           PERFORM 25530-PROCESSAR-CSR05   UNTIL
                   WRK-FINALIZAR           EQUAL 'S'.

           PERFORM 25540-FECHAR-CSR05.

      *----------------------------------------------------------------*
       25500-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRE O CURSOR 5                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       25510-ABRIR-CSR05               SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTL3-PONTEIRO-TARIFA TO CSERVC-TARIF      OF GFCTB0C6.
           MOVE WRK-DATA-DB2           TO DFIM-PRMSS-PCOTE  OF GFCTB0C6.
           MOVE 'N'                    TO WRK-FINALIZAR.

           EXEC SQL
               OPEN CSR05-GFCTB0C6
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PRMSS_ADSAO_PCOTE' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0150'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTL4-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0C6'         TO WRK-NOM-TAB
               PERFORM 24100-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       25510-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EFETUA LEITURA NO CURSOR 5                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       25520-LER-CSR05                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH   CSR05-GFCTB0C6   INTO
                   :GFCTB0C6.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PRMSS_ADSAO_PCOTE' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0160'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTL4-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0C6'         TO WRK-NOM-TAB
               PERFORM 24100-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTL4-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO GFCTL4-ERRO
                   MOVE ZEROS          TO GFCTL4-COD-SQL-ERRO
                   MOVE 0702           TO GFCTL4-COD-MSG-ERRO
                   MOVE '0702'         TO GFCTG2-COD-MSG
                   MOVE 'GFCTB0C6'     TO WRK-NOM-TAB
                   PERFORM 13100-OBTER-DESC-MSG
                   STRING GFCTG3-DESC-MSG ' - ' WRK-NOM-TAB
                   DELIMITED BY '  '           INTO GFCTL4-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       25520-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSA O CURSOR 5                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       25530-PROCESSAR-CSR05           SECTION.
      *----------------------------------------------------------------*

           PERFORM 25131-ACESSAR-GFCTB0D8.

           IF  WRK-5507-IDTFD-MASTER   EQUAL 'N'
               IF  CDEPDC              OF GFCTB0D8
                                       NOT EQUAL WRK-COD-JUNCAO-0315
                   PERFORM 25520-LER-CSR05
                   GO TO 25530-99-FIM
               END-IF
           END-IF.

           COMPUTE GFCTL4-LL           = GFCTL4-LL + 106.

           ADD 1                       TO GFCTL4-QTDE-TOT-REG
                                          IND-1.

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO GFCTL4-COD-TARIFA(IND-1).

           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO GFCTL4-DESC-TARIFA(IND-1).
           MOVE 'H'                    TO GFCTL4-TIPO-VIGENCIA(IND-1).

           PERFORM 25520-LER-CSR05.

           IF  WRK-FINALIZAR           EQUAL 'S'
               GO TO 25530-99-FIM
           END-IF.

           IF  IND-1                   GREATER 6
               MOVE 'S'                TO WRK-FINALIZAR
               MOVE CSERVC-TARIF       OF GFCTB0C6
                                       TO GFCTL4-PONTEIRO-TARIFA
           END-IF.

      *----------------------------------------------------------------*
       25530-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHA O CURSOR 5                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       25540-FECHAR-CSR05              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE   CSR05-GFCTB0C6
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SERVC_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0170'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTL4-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0C6'         TO WRK-NOM-TAB
               PERFORM 24100-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       25540-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *TRATAR CURSOR 6                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       25600-TRATAR-CSR06              SECTION.
      *----------------------------------------------------------------*

           PERFORM 25610-ABRIR-CSR06.

           PERFORM 25620-LER-CSR06.

           PERFORM 25630-PROCESSAR-CSR06   UNTIL
                   WRK-FINALIZAR           EQUAL 'S'.

           PERFORM 25640-FECHAR-CSR06.

      *----------------------------------------------------------------*
       25600-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRE O CURSOR 6                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       25610-ABRIR-CSR06               SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTL3-PONTEIRO-TARIFA TO CSERVC-TARIF      OF GFCTB0C6.
           MOVE WRK-DATA-DB2           TO DINIC-PRMSS-PCOTE OF GFCTB0C6.
           MOVE 'N'                    TO WRK-FINALIZAR.

           EXEC SQL
               OPEN CSR06-GFCTB0C6
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PRMSS_ADSAO_PCOTE' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0180'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTL4-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0C6'         TO WRK-NOM-TAB
               PERFORM 24100-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       25610-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EFETUA LEITURA NO CURSOR 6                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       25620-LER-CSR06                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH   CSR06-GFCTB0C6    INTO
                   :GFCTB0C6.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PRMSS_ADSAO_PCOTE' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0190'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTL4-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0C6'         TO WRK-NOM-TAB
               PERFORM 24100-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTL4-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO GFCTL4-ERRO
                   MOVE ZEROS          TO GFCTL4-COD-SQL-ERRO
                   MOVE 0703           TO GFCTL4-COD-MSG-ERRO
                   MOVE '0703'         TO GFCTG2-COD-MSG
                   MOVE 'GFCTB0C6'     TO WRK-NOM-TAB
                   PERFORM 13100-OBTER-DESC-MSG
                   STRING GFCTG3-DESC-MSG ' - ' WRK-NOM-TAB
                   DELIMITED BY '  '           INTO GFCTL4-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       25620-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSA O CURSOR 6                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       25630-PROCESSAR-CSR06           SECTION.
      *----------------------------------------------------------------*

           PERFORM 25131-ACESSAR-GFCTB0D8.

           IF  WRK-5507-IDTFD-MASTER   EQUAL 'N'
               IF  CDEPDC              OF GFCTB0D8
                                       NOT EQUAL WRK-COD-JUNCAO-0315
                   PERFORM 25620-LER-CSR06
                   GO TO 25630-99-FIM
               END-IF
           END-IF.

           COMPUTE GFCTL4-LL           = GFCTL4-LL + 106.

           ADD 1                       TO GFCTL4-QTDE-TOT-REG
                                          IND-1.

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO GFCTL4-COD-TARIFA(IND-1).
           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO GFCTL4-DESC-TARIFA(IND-1).
           MOVE 'F'                    TO GFCTL4-TIPO-VIGENCIA(IND-1).

           PERFORM 25620-LER-CSR06.

           IF  WRK-FINALIZAR           EQUAL 'S'
               GO TO 25630-99-FIM
           END-IF.

           IF  IND-1                   GREATER 6
               MOVE 'S'                TO WRK-FINALIZAR
               MOVE CSERVC-TARIF       OF GFCTB0C6
                                       TO GFCTL4-PONTEIRO-TARIFA
           END-IF.

      *----------------------------------------------------------------*
       25630-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHA O CURSOR 6                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       25640-FECHAR-CSR06              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE   CSR06-GFCTB0C6
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SERVC_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0200'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTL4-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0C6'         TO WRK-NOM-TAB
               PERFORM 24100-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       25640-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FINALIZA O PROCESSAMENTO E RETORNA AO PROGRAMA CHAMADOR.        *
      ******************************************************************
      *----------------------------------------------------------------*
       30000-FINALIZAR                 SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       30000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
