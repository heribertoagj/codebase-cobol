      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT6206.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      ******************************************************************
      *   A L E R T A     A L E R T A     A L E R T A     A L E R T A  *
      *                                                                *
      * ESTE PROGRAMA UTILIZA LETRAS EM MINUSCULO P/ CAMPO  AMBIENTE   *
      *                                                                *
      *  AO ALTERAR O PROGRAMA NO TSO UTILIZAR CAPS OFF E CAPS ON      *
      *  AO ALTERAR O PROGRAMA NO ROSCOE UTILIZAR SET MODE X           *
      *                                                                *
      *  MAS PRINCIPALMENTE VALIDAR ANTES DE ENVIAR PARA A PRODUCAO    *
      *  SE AS COMPARACOES DO CAMPO AMBIENTE ESTAO CORRETAS COM b E o  *
      *                                                                *
      *   A L E R T A     A L E R T A     A L E R T A     A L E R T A  *
      ******************************************************************
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT6206                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - PROCWORK/GP50 *
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - PROCWORK/GP50 *
      *    DATA........:   29/02/2008                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   MODULO BUSCADOR DE PACOTES VIGENTES E       *
      *                    FUTUROS.                                    *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                     INCLUDE/BOOK        *
      *                   DB2PRD.PARM_DATA_PROCM     GFCTB0A1          *
      *                   DB2PRD.PARM_SERVC_TARIF    GFCTB0A2          *
      *                   DB2PRD.PRMSS_ADSAO_PCOTE   GFCTB0C6          *
      *                   DB2PRD.PRMSS_GRP_DEPDC     GFCTB0C7          *
      *                   DB2PRD.PRMSS_GRP_EMPR      GFCTB0C8          *
      *                   DB2PRD.PRMSS_GRP_MUN       GFCTB0C9          *
      *                   DB2PRD.PRMSS_GRP_PAB       GFCTB0D0          *
      *                   DB2PRD.PRMSS_GRP_PSTAL     GFCTB0D1          *
      *                   DB2PRD.PRMSS_GRP_SGMTO     GFCTB0D2          *
      *                   DB2PRD.PRMSS_GRP_UF        GFCTB0D3          *
      *                   DB2PRD.SERVC_TARIF_PRINC   GFCTB0D8          *
      *                   DB2PRD.TPRMSS_GRP_CLI      GFCTB0I0          *
      *                   DB2PRD.V01CLIENTE_AGENCIA  CLIEV004          *
      *                   DB2PRD.V01CTA_POUPANCA     CLIEV007          *
      *                   DB2PRD.V01CTA_CORRENTE     CLIEV008          *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTV9 - ENTRADA - VERIFICAR PACOTES/TARIFAS VIGENTES.    *
      *    I#GFCTV9 - SAIDA   - VERIFICAR PACOTES/TARIFAS VIGENTES.    *
      *    I#GFCTG2 - ENTRADA - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTG3 - SAIDA   - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTJM - ENTRADA E SAIDA - OBTER COD SEG GESTAO TARIFA.   *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *    I#GFCTRK - ENTRADA E SAIDA - OBTER COD UF E MUNICIPIO DA    *
      *               AGENCIA.                                         *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    GFCT5527 - OBTER CPF/CNPJ/POSTO DE SERVICO.                 *
      *    GFCT5534 - OBTER CODIGO SEGMENTO GESTAO TARIFA.             *
      *    GFCT5564 - OBTER COD DA UF E MUNICIPIO DA AGENCIA.          *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                  *
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
       77  WRK-TIPO-PESSOA-CLIENTE     PIC  X(001)         VALUE SPACES.
       77  WRK-TIPO-CONTA-CLIENTE      PIC  9(001)         VALUE ZEROS.
       77  WRK-RESTO                   PIC  9(003)         VALUE ZEROS.
       77  WRK-INTEIRO                 PIC  9(004)         VALUE ZEROS.
       77  WRK-FIM-CURSOR-1            PIC  X(001)         VALUE SPACES.
       77  WRK-TARIFA-VALIDA           PIC  X(001)         VALUE SPACES.
       77  WRK-FIM-AGRUPAMENTO         PIC  X(001)         VALUE SPACES.
       77  WRK-EXISTE-AGRUPAMENTO      PIC  X(001)         VALUE SPACES.
       77  WRK-AGRUPAMENTO-VALIDO      PIC  X(001)         VALUE SPACES.
       77  WRK-PROX-AGRUPAMENTO        PIC  9(003)         VALUE ZEROS.
       77  WRK-SEQ-AGPTO               PIC S9(009) COMP-3  VALUE ZEROS.
       77  WRK-DT-INICIO               PIC  X(010)         VALUE SPACES.
       77  WRK-1-VEZ-SEGMENTO          PIC  X(001)         VALUE 'N'.
       77  WRK-1-VEZ-CNPJ-CPF          PIC  X(001)         VALUE 'N'.
       77  WRK-1-VEZ-UF-MUNIC          PIC  X(001)         VALUE 'N'.
       77  WRK-CESTA-CELULAR           PIC  X(001)         VALUE SPACES.
       77  WRK-COUNT                   PIC S9(009) COMP    VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA INDEXADORES *'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(005) COMP-3  VALUE ZEROS.
       77  WRK-IND-Z                   PIC  9(005) COMP-3  VALUE ZEROS.

       01  WRK-DT-PROCM-ATUAL          PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-PROCM-ATUAL-R        REDEFINES WRK-DT-PROCM-ATUAL.
           03  WRK-ANO-PROCM-ATUAL     PIC  9(004).
           03  WRK-MES-PROCM-ATUAL     PIC  9(002).
           03  WRK-DIA-PROCM-ATUAL     PIC  9(002).

       01  WRK-DT-INI-PRMSS-PCOTE      PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-INI-PRMSS-PCOTE-R    REDEFINES WRK-DT-INI-PRMSS-PCOTE.
           03  WRK-ANO-INI-PRMSS-PCOTE PIC  9(004).
           03  WRK-MES-INI-PRMSS-PCOTE PIC  9(002).
           03  WRK-DIA-INI-PRMSS-PCOTE PIC  9(002).

       01  WRK-DT-INI-ADSAO-INDVD      PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-INI-ADSAO-INDVD-R    REDEFINES WRK-DT-INI-ADSAO-INDVD.
           03  WRK-AAAAMM-INI-ADSAO-INDVD
                                       PIC  9(006).
           03  FILLER                  REDEFINES
                                             WRK-AAAAMM-INI-ADSAO-INDVD.
               05  WRK-ANO-INI-ADSAO-INDVD
                                       PIC  9(004).
               05  WRK-MES-INI-ADSAO-INDVD
                                       PIC  9(002).
           03  WRK-DIA-INI-ADSAO-INDVD PIC  9(002).

       01  WRK-DT-FIM-ADSAO-INDVD      PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-FIM-ADSAO-INDVD-R    REDEFINES WRK-DT-FIM-ADSAO-INDVD.
           03  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       PIC  9(006).
           03  FILLER                  REDEFINES
                                             WRK-AAAAMM-FIM-ADSAO-INDVD.
               05  WRK-ANO-FIM-ADSAO-INDVD
                                       PIC  9(004).
               05  WRK-MES-FIM-ADSAO-INDVD
                                       PIC  9(002).
           03  WRK-DIA-FIM-ADSAO-INDVD PIC  9(002).

       01  WRK-CINDCD-ADSAO-INDVD      PIC  X(001)         VALUE 'I'.
       01  WRK-CINDCD-ADSAO-AMBAS      PIC  X(001)         VALUE 'A'.

       01  WRK-MSG-SCCE9100.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA AO MODULO '.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5527 *'.
      *----------------------------------------------------------------*

       01  WRK-5527-AREA-ENTRADA.
           05  WRK-5527-AMBIENTE           PIC  X(001).
           05  WRK-5527-CHAMADOR           PIC  X(008).
           05  WRK-5527-AGENCIA-CLIENTE    PIC  9(005).
           05  WRK-5527-CONTA-CLIENTE      PIC  9(013).

       01  WRK-5527-AREA-SAIDA.
           05  WRK-5527-COD-RETORNO        PIC  9(002).
           05  WRK-5527-COD-SQL-ERRO       PIC S9(003).
           05  WRK-5527-COD-MSG-GFCT       PIC  9(004).
           05  WRK-5527-DESC-MSG           PIC  X(070).
           05  WRK-5527-QTD-OCORR          PIC  9(005).
           05  WRK-5527-DADOS-RETORNO.
ST25X6*      10  WRK-5527-CCGC-CPF         PIC  9(009).
ST25X6       10  WRK-5527-CCGC-CPF         PIC  X(009).
ST25X6*      10  WRK-5527-CFLIAL-CGC       PIC  9(004).
ST25X6       10  WRK-5527-CFLIAL-CGC       PIC  X(004).
             10  WRK-5527-CCTRL-CPF-CGC    PIC  9(002).
             10  WRK-5527-CPOSTO-SERVC     PIC  9(003).
             10  WRK-5527-CSGMTO-CLI       PIC  9(003).

       01  WRK-IO-PCB.
           03  WRK-IO-TERM              PIC  X(008).
           03  FILLER                   PIC  X(002).
           03  WRK-IO-STA               PIC  X(002).
           03  FILLER                   PIC  X(012).
           03  WRK-IO-MODNAME           PIC  X(008).

       01  WRK-ALT-PCB.
           03  WRK-ALT-PCBNAME          PIC  X(008).
           03  FILLER                   PIC  X(002).
           03  WRK-ALT-STATUS           PIC  X(002).
           03  FILLER                   PIC  X(012).
           03  WRK-ALT-MODNAME          PIC  X(008).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA PARA MENSAGENS MODULO GFCT5523 - MENSAGENS *'.
      *----------------------------------------------------------------*

       01  WRK-MSG-1                    PIC  X(070) VALUE
           'ADESAO AO PACOTE - NAO FOI POSSIVEL OBTER A MSG NO MODULO
      -    'GFCT5523'.

       01  WRK-MSG-2                    PIC  X(070) VALUE
           'ADESAO AO PACOTE - PROBLEMA NO ACESSO A BASE DE MENSAGENS'.

       01  WRK-MSG-3                    PIC  X(070) VALUE
           'ADESAO AO PACOTE - MSG NAO ENCONTRADA NA BASE DE MENSAGENS'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TRATAMENTO DE ERRO *'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA INCLUDES *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

       COPY 'I#GFCTJM'.

       COPY 'I#GFCTRK'.

       COPY 'I#GFCT0M'.

       COPY 'I#SCCE91'.

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
             INCLUDE GFCTB0A2
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
             INCLUDE CLIEV004
           END-EXEC.

           EXEC SQL
             INCLUDE CLIEV007
           END-EXEC.

           EXEC SQL
             INCLUDE CLIEV008
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D8
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DECLARACAO DE CURSORES *'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR01-GFCTB0C6 CURSOR FOR
             SELECT DISTINCT(A.CSERVC_TARIF)
             FROM  DB2PRD.PRMSS_ADSAO_PCOTE A,
                   DB2PRD.SERVC_TARIF_PRINC B
             WHERE
                   A.CSERVC_TARIF       = B.CSERVC_TARIF             AND
                  (A.CINDCD_ADSAO_INDVD = :WRK-CINDCD-ADSAO-INDVD    OR
                   A.CINDCD_ADSAO_INDVD = :WRK-CINDCD-ADSAO-AMBAS)   AND
                   A.DFIM_PRMSS_PCOTE   > :GFCTB0C6.DFIM-PRMSS-PCOTE AND
                   A.CSERVC_TARIF      >= :GFCTB0C6.CSERVC-TARIF     AND
                   B.CTPO_SERVC_TARIF   = 4
             ORDER BY CSERVC_TARIF
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTV9'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTV9-REG.
      *================================================================*

      ******************************************************************
      * ROTINA PRINCIPAL.                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           IF GFCTV9-AMBIENTE EQUAL 'B' OR 'b'
               CALL 'POOL1050'
           END-IF.

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA INICIALIZACAO DO PROGRAMA.                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO IND-1.

           MOVE ZEROS                  TO GFCTV9-COD-RETORNO
                                          GFCTV9-COD-MENSAGEM.
           MOVE SPACES                 TO GFCTV9-MENSAGEM
                                          GFCTV9-SQLCA.


           PERFORM VARYING WRK-IND-Z FROM 1 BY 1 UNTIL
                           WRK-IND-Z GREATER 5
               MOVE ZEROS  TO   GFCTV9-TARIFA      (WRK-IND-Z)
               MOVE SPACES TO   GFCTV9-NOME-TARIFA (WRK-IND-Z)
               MOVE SPACES TO   GFCTV9-DATA-INICIO (WRK-IND-Z)
               MOVE SPACES TO   GFCTV9-DATA-FIM    (WRK-IND-Z)
               MOVE SPACES TO   GFCTV9-CESTA-CELULAR (WRK-IND-Z)
           END-PERFORM

           PERFORM 1100-CONSISTIR-DADOS.

           PERFORM 3100-VALIDAR-CONTA.

           PERFORM 3200-CONSISTE-AG-CTA.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONSISTENCIA DOS DADOS DE ENTRADA.                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (GFCTV9-AGENCIA           NOT NUMERIC)                OR
              (GFCTV9-AGENCIA           EQUAL ZEROS)                OR
              (GFCTV9-CONTA             NOT NUMERIC)                OR
              (GFCTV9-CONTA             EQUAL ZEROS)                OR
              (GFCTV9-PONTEIRO          NOT NUMERIC)                OR
              (GFCTV9-AMBIENTE          NOT EQUAL 'O' AND 'B'
                                              AND 'o' AND 'b')
               MOVE 'B'                 TO GFCTV9-AMBIENTE
               MOVE 1166                TO GFCTG2-COD-MSG
                                           GFCTV9-COD-MENSAGEM
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 1                   TO GFCTV9-COD-RETORNO
               MOVE GFCTG3-DESC-MSG     TO GFCTV9-MENSAGEM
               MOVE SPACES              TO GFCTV9-SQLCA
               MOVE 12                  TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF GFCTV9-AMBIENTE EQUAL 'o'
              MOVE 'O' TO GFCTV9-AMBIENTE
           END-IF.

           IF GFCTV9-AMBIENTE EQUAL 'b'
              MOVE 'B' TO GFCTV9-AMBIENTE
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA ACESSO AO MODULO DE DESCRICOES DE MENSAGENS.       *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE 'GFCT6206'             TO GFCTG2-TRANSACAO
                                          GFCTG2-FUNCAO.
           MOVE '1'                    TO GFCTG2-FUNC-BDSCO.
           MOVE GFCTV9-AMBIENTE        TO GFCTG2-TIPO-PROC.
           MOVE 'N'                    TO GFCTG2-FIM.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 2                  TO GFCTV9-COD-RETORNO
               MOVE 1305               TO GFCTV9-COD-MENSAGEM
               MOVE WRK-MSG-1          TO GFCTV9-MENSAGEM
               MOVE SPACES             TO GFCTV9-SQLCA
               MOVE 04                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 3                  TO GFCTV9-COD-RETORNO
               MOVE 1306               TO GFCTV9-COD-MENSAGEM
               MOVE WRK-MSG-2          TO GFCTV9-MENSAGEM
               MOVE GFCT0M-SQLCA-AREA  TO GFCTV9-SQLCA
               MOVE 04                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE 4                  TO GFCTV9-COD-RETORNO
               MOVE 1307               TO GFCTV9-COD-MENSAGEM
               MOVE WRK-MSG-3          TO GFCTV9-MENSAGEM
               MOVE SPACES             TO GFCTV9-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA DE PROCESSAMENTO PRINCIPAL DO PROGRAMA.                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO IND-1.
           MOVE 'N'                    TO WRK-FIM-CURSOR-1.

           PERFORM 2100-OBTER-DATA-PROCESSAMENTO.

           PERFORM 2200-ABRIR-CURSOR-1.

           PERFORM 2300-LER-CURSOR-1.

           PERFORM 2400-PROCESSAR-CURSOR-1 UNTIL
                   WRK-FIM-CURSOR-1        EQUAL 'S'.

           IF GFCTV9-PONTEIRO   EQUAL   ZEROS
               IF GFCTV9-AMBIENTE EQUAL 'O'
                   MOVE 1337               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               ELSE
                   MOVE 1199               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 34                 TO GFCTV9-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTV9-MENSAGEM
               MOVE ZEROS              TO RETURN-CODE
           END-IF.

           PERFORM 2500-FECHAR-CURSOR-1.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA OBTER A DATA DE PROCESSAMENTO.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-OBTER-DATA-PROCESSAMENTO   SECTION.
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
               IF GFCTV9-AMBIENTE EQUAL 'O'
                   MOVE 1309               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               ELSE
                   MOVE 1170               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 6                  TO GFCTV9-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTV9-MENSAGEM
               MOVE SQLCA              TO GFCTV9-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ABRIR CURSOR 1 DA TABELA GFCTB0C6                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-ABRIR-CURSOR-1             SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTV9-PONTEIRO        TO CSERVC-TARIF      OF GFCTB0C6.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DFIM-PRMSS-PCOTE  OF GFCTB0C6.

           MOVE 'I'                    TO WRK-CINDCD-ADSAO-INDVD.
           MOVE 'A'                    TO WRK-CINDCD-ADSAO-AMBAS.

           EXEC SQL
               OPEN CSR01-GFCTB0C6
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               IF GFCTV9-AMBIENTE EQUAL 'O'
                   MOVE 1310               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               ELSE
                   MOVE 1171               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 7                  TO GFCTV9-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTV9-MENSAGEM
               MOVE SQLCA              TO GFCTV9-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * LER CURSOR 1 DA TABELA GFCTB0C6                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-LER-CURSOR-1               SECTION.
      *----------------------------------------------------------------*


       2300-LER-CURSOR-A.


           EXEC SQL
             FETCH CSR01-GFCTB0C6 INTO
                   :GFCTB0C6.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               IF GFCTV9-AMBIENTE EQUAL 'O'
                   MOVE 1311               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               ELSE
                   MOVE 1172               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 8                  TO GFCTV9-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTV9-MENSAGEM
               MOVE SQLCA              TO GFCTV9-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR-1
               IF  GFCTV9-PONTEIRO     NOT EQUAL ZEROS
                   MOVE ZEROS          TO GFCTV9-PONTEIRO
               END-IF
               IF  IND-1               EQUAL ZEROS
                   PERFORM 2500-FECHAR-CURSOR-1

                   IF GFCTV9-AMBIENTE EQUAL 'O'
                      MOVE '1312'             TO GFCTG2-COD-MSG
                      MOVE  1312              TO GFCTV9-COD-MENSAGEM
                   ELSE
                      MOVE '1173'             TO GFCTG2-COD-MSG
                      MOVE  1173              TO GFCTV9-COD-MENSAGEM
                   END-IF
                   PERFORM 1200-OBTER-DESC-MSG
                   MOVE 9                  TO GFCTV9-COD-RETORNO
                   MOVE GFCTG3-DESC-MSG    TO GFCTV9-MENSAGEM
                   MOVE SQLCA              TO GFCTV9-SQLCA
                   MOVE 04                 TO RETURN-CODE
                   PERFORM 4000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA ACESSAR GFCTB0A2                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2310-ACESSAR-GFCTB0A2       SECTION.
      *----------------------------------------------------------------*

           MOVE  CSERVC-TARIF OF GFCTB0C6
                                   TO  CSERVC-TARIF        OF GFCTB0A2.
           MOVE  DPROCM-ATUAL OF GFCTB0A1
                                   TO  DINIC-VGCIA-TARIF   OF GFCTB0A2.
           MOVE  DPROCM-ATUAL OF GFCTB0A1
                                   TO  DFIM-VGCIA-TARIF    OF GFCTB0A2.

           EXEC SQL
                SELECT
                     CINDCD_CREDT_CLULR
                INTO
                    :GFCTB0A2.CINDCD-CREDT-CLULR
                FROM   DB2PRD.PARM_SERVC_TARIF
                WHERE  CSERVC_TARIF       = :GFCTB0A2.CSERVC-TARIF
                  AND  DINIC_VGCIA_TARIF <= :GFCTB0A2.DINIC-VGCIA-TARIF
                  AND  DFIM_VGCIA_TARIF  >= :GFCTB0A2.DFIM-VGCIA-TARIF
           END-EXEC.

           IF  (SQLCODE                  NOT EQUAL ZEROS AND +100 ) OR
               (SQLWARN0                 EQUAL 'W')
               IF GFCTV9-AMBIENTE EQUAL 'O'
                   MOVE '1318'              TO GFCTG2-COD-MSG
                   MOVE 1318                TO GFCTV9-COD-MENSAGEM
               ELSE
                   MOVE '1198'              TO GFCTG2-COD-MSG
                   MOVE 1198                TO GFCTV9-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 33                  TO GFCTV9-COD-RETORNO
               MOVE GFCTG3-DESC-MSG     TO GFCTV9-MENSAGEM
               MOVE SQLCA               TO GFCTV9-SQLCA
               MOVE 12                  TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  CINDCD-CREDT-CLULR OF GFCTB0A2   EQUAL   'S'
               MOVE 'S'                TO WRK-CESTA-CELULAR
           ELSE
               MOVE 'N'                TO WRK-CESTA-CELULAR
           END-IF.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR DADOS OBTIDOS NO CURSOR 1 DA TABELA GFCTB0C6.        *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-PROCESSAR-CURSOR-1         SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-TARIFA-VALIDA.

           PERFORM 2410-OBTER-DADOS-GFCTB0D8.

           IF  CTPO-CTA-DEB-TARIF      OF GFCTB0D8
                                       EQUAL WRK-TIPO-CONTA-CLIENTE
               IF  CPSSOA-SERVC-TARIF  OF GFCTB0D8 EQUAL 3    OR
                  (CPSSOA-SERVC-TARIF  OF GFCTB0D8 EQUAL 1    AND
                   WRK-TIPO-PESSOA-CLIENTE       EQUAL 'F')   OR
                  (CPSSOA-SERVC-TARIF  OF GFCTB0D8 EQUAL 2    AND
                   WRK-TIPO-PESSOA-CLIENTE       EQUAL 'J')
                   PERFORM 2420-CALCULAR-DATA-FIM

                   MOVE 014            TO WRK-PROX-AGRUPAMENTO

                   MOVE 'N'            TO WRK-FIM-AGRUPAMENTO

                   MOVE 'N'            TO WRK-AGRUPAMENTO-VALIDO

                   PERFORM 2430-IDENTIFICAR-PERMISSAO UNTIL
                           WRK-FIM-AGRUPAMENTO        EQUAL 'S' OR
                           WRK-AGRUPAMENTO-VALIDO     EQUAL 'S'

                   IF  WRK-AGRUPAMENTO-VALIDO         EQUAL 'S'
                       MOVE 'S'        TO WRK-TARIFA-VALIDA
                   END-IF
               END-IF
           END-IF.

           IF  WRK-TARIFA-VALIDA       EQUAL 'S'
               PERFORM 2470-MONTAR-OCORRENCIAS
           ELSE
               PERFORM 2300-LER-CURSOR-1
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER DADOS DA TARIFA.                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2410-OBTER-DADOS-GFCTB0D8       SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF     OF GFCTB0D8.

           EXEC SQL
             SELECT
                   RSERVC_TARIF_REDZD,
                   CPSSOA_SERVC_TARIF,
                   CTPO_CTA_DEB_TARIF
             INTO
                   :GFCTB0D8.RSERVC-TARIF-REDZD,
                   :GFCTB0D8.CPSSOA-SERVC-TARIF,
                   :GFCTB0D8.CTPO-CTA-DEB-TARIF
             FROM   DB2PRD.SERVC_TARIF_PRINC
             WHERE
                   CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               IF GFCTV9-AMBIENTE EQUAL 'O'
                   MOVE 1317               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               ELSE
                   MOVE 1178               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 14                 TO GFCTV9-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTV9-MENSAGEM
               MOVE SQLCA              TO GFCTV9-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CALCULAR A DATA DE FIM DA ADESAO INDIVIDUAL.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2420-CALCULAR-DATA-FIM          SECTION.
      *----------------------------------------------------------------*

           PERFORM 2421-OBTER-DADOS-GFCTB0A2.

           MOVE DFIM-VGCIA-TARIF       OF GFCTB0A2(1:2)
                                       TO WRK-DIA-FIM-ADSAO-INDVD.
           MOVE DFIM-VGCIA-TARIF       OF GFCTB0A2(4:2)
                                       TO WRK-MES-FIM-ADSAO-INDVD.
           MOVE DFIM-VGCIA-TARIF       OF GFCTB0A2(7:4)
                                       TO WRK-ANO-FIM-ADSAO-INDVD.

           IF (WRK-MES-FIM-ADSAO-INDVD EQUAL 01 OR 03 OR 05 OR 07 OR
                                             08 OR 10 OR 12) AND
               WRK-DIA-FIM-ADSAO-INDVD EQUAL 31
              NEXT SENTENCE
           ELSE
              IF (WRK-MES-FIM-ADSAO-INDVD EQUAL 04 OR 06 OR
                                                09 OR 11) AND
                  WRK-DIA-FIM-ADSAO-INDVD EQUAL 30
                 NEXT SENTENCE
              ELSE
                 IF WRK-MES-FIM-ADSAO-INDVD EQUAL 02 AND
                    WRK-DIA-FIM-ADSAO-INDVD EQUAL 29
                    NEXT SENTENCE
                 ELSE
                    DIVIDE WRK-ANO-FIM-ADSAO-INDVD BY 4
                                           GIVING WRK-INTEIRO
                                           REMAINDER WRK-RESTO
                    IF WRK-MES-FIM-ADSAO-INDVD EQUAL 02 AND
                       WRK-RESTO               NOT EQUAL ZEROS AND
                       WRK-DIA-FIM-ADSAO-INDVD EQUAL 28
                       NEXT SENTENCE
                    ELSE
                       SUBTRACT 1          FROM WRK-MES-FIM-ADSAO-INDVD

                       EVALUATE WRK-MES-FIM-ADSAO-INDVD

                       WHEN 00
                            MOVE 31        TO WRK-DIA-FIM-ADSAO-INDVD
                            MOVE 12        TO WRK-MES-FIM-ADSAO-INDVD
                            SUBTRACT 1     FROM WRK-ANO-FIM-ADSAO-INDVD

                       WHEN 01
                       WHEN 03
                       WHEN 05
                       WHEN 07
                       WHEN 08
                       WHEN 10
                       WHEN 12
                            MOVE 31        TO WRK-DIA-FIM-ADSAO-INDVD

                       WHEN 04
                       WHEN 06
                       WHEN 09
                       WHEN 11
                            MOVE 30        TO WRK-DIA-FIM-ADSAO-INDVD

                       WHEN 02
                            IF WRK-RESTO   EQUAL ZEROS
                               MOVE 28     TO WRK-DIA-FIM-ADSAO-INDVD
                            ELSE
                               MOVE 29     TO WRK-DIA-FIM-ADSAO-INDVD
                            END-IF
                       END-EVALUATE
                    END-IF
                 END-IF
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       2420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER DADOS DO PARAMETRO DA TARIFA.                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2421-OBTER-DADOS-GFCTB0A2       SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0C6 TO CSERVC-TARIF   OF GFCTB0A2.

           EXEC SQL
             SELECT MAX(DFIM_VGCIA_TARIF)
             INTO
                   :GFCTB0A2.DFIM-VGCIA-TARIF
             FROM   DB2PRD.PARM_SERVC_TARIF
             WHERE
                   CSERVC_TARIF       = :GFCTB0A2.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               IF GFCTV9-AMBIENTE EQUAL 'O'
                   MOVE 1318               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               ELSE
                   MOVE 1179               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 15                 TO GFCTV9-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTV9-MENSAGEM
               MOVE SQLCA              TO GFCTV9-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2421-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * IDENTIFICAR AGRUPAMENTO COM PERMISSAO DE ADESAO AO PACOTE.     *
      ******************************************************************
      *----------------------------------------------------------------*
       2430-IDENTIFICAR-PERMISSAO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-EXISTE-AGRUPAMENTO.

           EVALUATE WRK-PROX-AGRUPAMENTO

             WHEN 004
                  IF  WRK-5527-CCGC-CPF               NOT EQUAL ZEROS
                      PERFORM 2432-ACESSAR-GFCTB0I0

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 2450-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 018         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 009
                  IF  WRK-1-VEZ-SEGMENTO  EQUAL  'N'
                      PERFORM 2433-OBTER-SEG-GESTAO
                      MOVE  'S'  TO  WRK-1-VEZ-SEGMENTO
                  END-IF

                  IF  GFCTJM-CSGMTO-GSTAO-TARIF       NOT EQUAL ZEROS
                      PERFORM 2434-ACESSAR-GFCTB0D2

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 2450-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 'S'         TO WRK-FIM-AGRUPAMENTO
                  END-IF

             WHEN 014
                  PERFORM 2435-ACESSAR-GFCTB0C7

                  IF  WRK-EXISTE-AGRUPAMENTO          EQUAL 'S'
                      PERFORM 2450-OBTER-DADOS-GFCTB0C6
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 015         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 015
                  IF  WRK-1-VEZ-CNPJ-CPF  EQUAL  'N'
                      PERFORM 2431-OBTER-CNPJ-CPF-POSTO
                      MOVE  'S'  TO  WRK-1-VEZ-CNPJ-CPF
                  END-IF

                  IF  WRK-5527-CPOSTO-SERVC           NOT EQUAL ZEROS
                      PERFORM 2436-ACESSAR-GFCTB0D0

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 2450-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 017         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 017
                  IF  WRK-5527-CPOSTO-SERVC           NOT EQUAL ZEROS
                      PERFORM 2437-ACESSAR-GFCTB0D1

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 2450-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 020         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 018
                  IF  WRK-1-VEZ-UF-MUNIC  EQUAL  'N'
                      PERFORM 2438-OBTER-UF-MUNICIPIO
                      MOVE  'S'  TO  WRK-1-VEZ-UF-MUNIC
                  END-IF

                  IF  GFCTRK-CMUN-RURAL               NOT EQUAL ZEROS
                      PERFORM 2439-ACESSAR-GFCTB0C9

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 2450-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 019         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 019
                  IF  GFCTRK-CSGL-UF                  NOT EQUAL SPACES
                      PERFORM 2440-ACESSAR-GFCTB0D3

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 2450-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 009         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 020
                  IF  WRK-5527-CPOSTO-SERVC           NOT EQUAL ZEROS
                      PERFORM 2437-ACESSAR-GFCTB0D1

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 2450-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 004         TO WRK-PROX-AGRUPAMENTO
                  END-IF

           END-EVALUATE.

      *----------------------------------------------------------------*
       2430-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSAR MODULO GFCT5527 PARA OBTER CPF/CNPJ/POSTO.             *
      ******************************************************************
      *----------------------------------------------------------------*
       2431-OBTER-CNPJ-CPF-POSTO       SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                     TO WRK-5527-AREA-SAIDA.

           INITIALIZE WRK-5527-AREA-SAIDA.

           MOVE 'GFCT6206'                 TO WRK-5527-CHAMADOR.
           MOVE GFCTV9-AGENCIA             TO WRK-5527-AGENCIA-CLIENTE.
           MOVE GFCTV9-CONTA               TO WRK-5527-CONTA-CLIENTE.
           MOVE GFCTV9-AMBIENTE            TO WRK-5527-AMBIENTE.

           MOVE 'GFCT5527'                 TO WRK-MODULO.

           CALL 'POOL0081'                       USING WRK-MODULO
                                                 WRK-5527-AREA-ENTRADA
                                                 WRK-5527-AREA-SAIDA
                                                 GFCT0M-AREA-ERROS
                                                 WRK-IO-PCB
                                                 WRK-ALT-PCB.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               IF GFCTV9-AMBIENTE EQUAL 'O'
                   MOVE 1319               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               ELSE
                   MOVE 1180               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 16                 TO GFCTV9-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTV9-MENSAGEM
               MOVE SPACES             TO GFCTV9-SQLCA
               MOVE 04                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  WRK-5527-COD-RETORNO        NOT EQUAL ZEROS AND 02
                                                           AND 05
               IF GFCTV9-AMBIENTE EQUAL 'O'
                   MOVE 1319                   TO GFCTG2-COD-MSG
                                                  GFCTV9-COD-MENSAGEM
               ELSE
                   MOVE 1181                   TO GFCTG2-COD-MSG
                                                  GFCTV9-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 17                     TO GFCTV9-COD-RETORNO
               MOVE GFCTG3-DESC-MSG        TO GFCTV9-MENSAGEM
               MOVE SPACES                 TO GFCTV9-SQLCA
               MOVE 04                     TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  WRK-5527-COD-RETORNO        EQUAL 02 OR 05
               MOVE ZEROS                  TO WRK-5527-CCGC-CPF
                                              WRK-5527-CFLIAL-CGC
                                              WRK-5527-CCTRL-CPF-CGC
           END-IF.

      *----------------------------------------------------------------*
       2431-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO CLIENTE.*
      ******************************************************************
      *----------------------------------------------------------------*
       2432-ACESSAR-GFCTB0I0           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0I0.
           MOVE WRK-PROX-AGRUPAMENTO
                                       TO CAGPTO-CTA        OF GFCTB0I0.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0I0.
ST25X6     MOVE WRK-5527-CCGC-CPF      TO CCGC-CPF-ST       OF GFCTB0I0.
ST25X6     MOVE WRK-5527-CFLIAL-CGC    TO CFLIAL-CGC-ST     OF GFCTB0I0.
ST25X6     MOVE WRK-5527-CCTRL-CPF-CGC TO CCTRL-CPF-CGC-ST  OF GFCTB0I0.

           EXEC SQL
             SELECT
                   CSEQ_AGPTO_CTA,
                   DINIC_PRMSS_PCOTE
             INTO
                   :GFCTB0I0.CSEQ-AGPTO-CTA,
                   :GFCTB0I0.DINIC-PRMSS-PCOTE
             FROM   DB2PRD.TPRMSS_GRP_CLI
             WHERE
                   CSERVC_TARIF       = :GFCTB0I0.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0I0.CAGPTO-CTA        AND
ST25X6*            CCGC_CPF           = :GFCTB0I0.CCGC-CPF          AND
ST25X6*            CFLIAL_CGC         = :GFCTB0I0.CFLIAL-CGC        AND
ST25X6*            CCTRL_CPF_CGC      = :GFCTB0I0.CCTRL-CPF-CGC     AND
ST25X6             CCGC_CPF_ST        = :GFCTB0I0.CCGC-CPF-ST       AND
ST25X6             CFLIAL_CGC_ST      = :GFCTB0I0.CFLIAL-CGC-ST     AND
ST25X6             CCTRL_CPF_CGC_ST   = :GFCTB0I0.CCTRL-CPF-CGC-ST  AND
                   DINIC_PRMSS_PCOTE  IN
                (SELECT MAX(DINIC_PRMSS_PCOTE)
                 FROM   DB2PRD.TPRMSS_GRP_CLI
                 WHERE
                   CSERVC_TARIF       = :GFCTB0I0.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0I0.CAGPTO-CTA        AND
ST25X6*            CCGC_CPF           = :GFCTB0I0.CCGC-CPF          AND
ST25X6*            CFLIAL_CGC         = :GFCTB0I0.CFLIAL-CGC        AND
ST25X6*            CCTRL_CPF_CGC      = :GFCTB0I0.CCTRL-CPF-CGC     AND
ST25X6             CCGC_CPF_ST        = :GFCTB0I0.CCGC-CPF-ST       AND
ST25X6             CFLIAL_CGC_ST      = :GFCTB0I0.CFLIAL-CGC-ST     AND
ST25X6             CCTRL_CPF_CGC_ST   = :GFCTB0I0.CCTRL-CPF-CGC-ST  AND
                   DINIC_PRMSS_PCOTE <= :GFCTB0I0.DINIC-PRMSS-PCOTE)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               IF GFCTV9-AMBIENTE EQUAL 'O'
                   MOVE 1324               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               ELSE
                   MOVE 1184               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 21                 TO GFCTV9-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTV9-MENSAGEM
               MOVE SQLCA              TO GFCTV9-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2460-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                         WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA    OF GFCTB0I0 TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0I0 TO WRK-DT-INICIO
                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2432-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSAR MODULO GFCT5534 PARA OBTER SEGMENTO GESTAO.            *
      ******************************************************************
      *----------------------------------------------------------------*
       2433-OBTER-SEG-GESTAO           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTJM-AREA.

           INITIALIZE GFCTJM-AREA.

           MOVE GFCTV9-AMBIENTE
                                TO GFCTJM-TPO-AMBIENTE.
           MOVE GFCTV9-AGENCIA  TO GFCTJM-CJUNC-DEPDC.
           MOVE GFCTV9-CONTA    TO GFCTJM-CCTA-CLI.

           MOVE 'GFCT5534'                 TO WRK-MODULO.

           CALL 'POOL0081'                 USING WRK-MODULO
                                                 GFCTJM-AREA
                                                 GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               IF GFCTV9-AMBIENTE EQUAL 'O'
                   MOVE 1326               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               ELSE
                   MOVE 1187               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 23                 TO GFCTV9-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTV9-MENSAGEM
               MOVE SPACES             TO GFCTV9-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  GFCTJM-COD-RETORNO      EQUAL 99
               IF GFCTV9-AMBIENTE EQUAL 'O'
                   MOVE 1327               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               ELSE
                   MOVE 1189               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 24                 TO GFCTV9-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTV9-MENSAGEM
               MOVE SPACES             TO GFCTV9-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  GFCTJM-COD-RETORNO         NOT EQUAL ZEROS
               MOVE ZEROS                 TO GFCTJM-CSGMTO-GSTAO-TARIF
           END-IF.

      *----------------------------------------------------------------*
       2433-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO SEGMENTO*
      ******************************************************************
      *----------------------------------------------------------------*
       2434-ACESSAR-GFCTB0D2           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D2.
           MOVE WRK-PROX-AGRUPAMENTO
                                       TO CAGPTO-CTA        OF GFCTB0D2.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0D2.
           MOVE GFCTJM-CSGMTO-GSTAO-TARIF
                                       TO CSGMTO-GSTAO-TARIF
                                                            OF GFCTB0D2.

           EXEC SQL
             SELECT
                   CSEQ_AGPTO_CTA,
                   DINIC_PRMSS_PCOTE
             INTO
                   :GFCTB0D2.CSEQ-AGPTO-CTA,
                   :GFCTB0D2.DINIC-PRMSS-PCOTE
             FROM   DB2PRD.PRMSS_GRP_SGMTO
             WHERE
                  CSERVC_TARIF       = :GFCTB0D2.CSERVC-TARIF       AND
                  CAGPTO_CTA         = :GFCTB0D2.CAGPTO-CTA         AND
                  CSGMTO_GSTAO_TARIF = :GFCTB0D2.CSGMTO-GSTAO-TARIF AND
                  DINIC_PRMSS_PCOTE  IN
                (SELECT MAX(DINIC_PRMSS_PCOTE)
                 FROM   DB2PRD.PRMSS_GRP_SGMTO
                 WHERE
                  CSERVC_TARIF       = :GFCTB0D2.CSERVC-TARIF       AND
                  CAGPTO_CTA         = :GFCTB0D2.CAGPTO-CTA         AND
                  CSGMTO_GSTAO_TARIF = :GFCTB0D2.CSGMTO-GSTAO-TARIF AND
                  DINIC_PRMSS_PCOTE <= :GFCTB0D2.DINIC-PRMSS-PCOTE)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               IF GFCTV9-AMBIENTE EQUAL 'O'
                   MOVE 1328               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               ELSE
                   MOVE 1190               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 25                 TO GFCTV9-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTV9-MENSAGEM
               MOVE SQLCA              TO GFCTV9-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2460-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                         WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA    OF GFCTB0D2 TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0D2 TO WRK-DT-INICIO
                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2434-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO AGENCIA.*
      ******************************************************************
      *----------------------------------------------------------------*
       2435-ACESSAR-GFCTB0C7           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0C7.
           MOVE WRK-PROX-AGRUPAMENTO
                                       TO CAGPTO-CTA        OF GFCTB0C7.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C7.
           MOVE GFCTV9-AGENCIA         TO CDEPDC            OF GFCTB0C7.

           EXEC SQL
             SELECT
                   CSEQ_AGPTO_CTA,
                   DINIC_PRMSS_PCOTE
             INTO
                   :GFCTB0C7.CSEQ-AGPTO-CTA,
                   :GFCTB0C7.DINIC-PRMSS-PCOTE
             FROM   DB2PRD.PRMSS_GRP_DEPDC
             WHERE
                   CSERVC_TARIF       = :GFCTB0C7.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0C7.CAGPTO-CTA        AND
                  (CDEPDC             = 0                           OR
                   CDEPDC             = :GFCTB0C7.CDEPDC)           AND
                   DINIC_PRMSS_PCOTE  IN
                (SELECT MAX(DINIC_PRMSS_PCOTE)
                 FROM   DB2PRD.PRMSS_GRP_DEPDC
                 WHERE
                   CSERVC_TARIF       = :GFCTB0C7.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0C7.CAGPTO-CTA        AND
                  (CDEPDC             = 0                           OR
                   CDEPDC             = :GFCTB0C7.CDEPDC)           AND
                   DINIC_PRMSS_PCOTE <= :GFCTB0C7.DINIC-PRMSS-PCOTE)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               IF GFCTV9-AMBIENTE EQUAL 'O'
                   MOVE 1329               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               ELSE
                   MOVE 1191               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 26                 TO GFCTV9-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTV9-MENSAGEM
               MOVE SQLCA              TO GFCTV9-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2460-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                         WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA    OF GFCTB0C7 TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0C7 TO WRK-DT-INICIO
                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2435-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO PAB.    *
      ******************************************************************
      *----------------------------------------------------------------*
       2436-ACESSAR-GFCTB0D0           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF                                OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D0.
           MOVE WRK-PROX-AGRUPAMENTO
                                       TO CAGPTO-CTA        OF GFCTB0D0.
           MOVE DPROCM-ATUAL                                OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0D0.
           MOVE GFCTV9-AGENCIA         TO CDEPDC            OF GFCTB0D0.

           MOVE WRK-5527-CPOSTO-SERVC  TO CPOSTO-SERVC      OF GFCTB0D0.

           EXEC SQL
             SELECT
                   CSEQ_AGPTO_CTA,
                   DINIC_PRMSS_PCOTE
             INTO
                   :GFCTB0D0.CSEQ-AGPTO-CTA,
                   :GFCTB0D0.DINIC-PRMSS-PCOTE
             FROM   DB2PRD.PRMSS_GRP_PAB
             WHERE
                   CSERVC_TARIF       = :GFCTB0D0.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0D0.CAGPTO-CTA        AND
                   CDEPDC             = :GFCTB0D0.CDEPDC            AND
                  (CPOSTO_SERVC       = 0                           OR
                   CPOSTO_SERVC       = :GFCTB0D0.CPOSTO-SERVC)     AND
                   DINIC_PRMSS_PCOTE  IN
                (SELECT MAX(DINIC_PRMSS_PCOTE)
                 FROM   DB2PRD.PRMSS_GRP_PAB
                 WHERE
                   CSERVC_TARIF       = :GFCTB0D0.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0D0.CAGPTO-CTA        AND
                   CDEPDC             = :GFCTB0D0.CDEPDC            AND
                  (CPOSTO_SERVC       = 0                           OR
                   CPOSTO_SERVC       = :GFCTB0D0.CPOSTO-SERVC)     AND
                   DINIC_PRMSS_PCOTE <= :GFCTB0D0.DINIC-PRMSS-PCOTE)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               IF GFCTV9-AMBIENTE EQUAL 'O'
                   MOVE 1330               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               ELSE
                   MOVE 1192               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 27                 TO GFCTV9-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTV9-MENSAGEM
               MOVE SQLCA              TO GFCTV9-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2460-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                         WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA    OF GFCTB0D0 TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0D0 TO WRK-DT-INICIO
                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2436-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO PSTAL.  *
      ******************************************************************
      *----------------------------------------------------------------*
       2437-ACESSAR-GFCTB0D1           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF                                OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D1.
           MOVE WRK-PROX-AGRUPAMENTO
                                       TO CAGPTO-CTA        OF GFCTB0D1.
           MOVE DPROCM-ATUAL                                OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0D1.
           MOVE GFCTV9-AGENCIA         TO CDEPDC            OF GFCTB0D1.
           MOVE WRK-5527-CPOSTO-SERVC  TO CPOSTO-SERVC      OF GFCTB0D1.

           EXEC SQL
             SELECT
                   CSEQ_AGPTO_CTA,
                   DINIC_PRMSS_PCOTE
             INTO
                   :GFCTB0D1.CSEQ-AGPTO-CTA,
                   :GFCTB0D1.DINIC-PRMSS-PCOTE
             FROM   DB2PRD.PRMSS_GRP_PSTAL
             WHERE
                   CSERVC_TARIF       = :GFCTB0D1.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0D1.CAGPTO-CTA        AND
                  (CDEPDC             = 0                           OR
                   CDEPDC             = :GFCTB0D1.CDEPDC            AND
                   CPOSTO_SERVC       = :GFCTB0D1.CPOSTO-SERVC)     AND
                   DINIC_PRMSS_PCOTE  IN
                (SELECT MAX(DINIC_PRMSS_PCOTE)
                 FROM   DB2PRD.PRMSS_GRP_PSTAL
                 WHERE
                   CSERVC_TARIF       = :GFCTB0D1.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0D1.CAGPTO-CTA        AND
                  (CDEPDC             = 0                           OR
                   CDEPDC             = :GFCTB0D1.CDEPDC            AND
                   CPOSTO_SERVC       = :GFCTB0D1.CPOSTO-SERVC)     AND
                   DINIC_PRMSS_PCOTE <= :GFCTB0D1.DINIC-PRMSS-PCOTE)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               IF GFCTV9-AMBIENTE EQUAL 'O'
                   MOVE 1331               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               ELSE
                   MOVE 1193               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 28                 TO GFCTV9-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTV9-MENSAGEM
               MOVE SQLCA              TO GFCTV9-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2460-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                         WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA    OF GFCTB0D1 TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0D1 TO WRK-DT-INICIO
                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2437-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSAR MODULO GFCT5564 PARA OBTER UF E MUNICIPIO.             *
      ******************************************************************
      *----------------------------------------------------------------*
       2438-OBTER-UF-MUNICIPIO         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTRK-AREA.

           INITIALIZE GFCTRK-AREA.

           MOVE GFCTV9-AMBIENTE        TO GFCTRK-TPO-AMBIENTE.
           MOVE GFCTV9-AGENCIA         TO GFCTRK-CJUNC-DEPDC.

           MOVE 'GFCT5564'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTRK-AREA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               IF GFCTV9-AMBIENTE EQUAL 'O'
                   MOVE 1332               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               ELSE
                   MOVE 1194               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 29                 TO GFCTV9-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTV9-MENSAGEM
               MOVE SPACES             TO GFCTV9-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  GFCTRK-COD-RETORNO      EQUAL 01 OR 02 OR 99
               IF GFCTV9-AMBIENTE EQUAL 'O'
                   MOVE 1333               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               ELSE
                   MOVE 1195               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 30                 TO GFCTV9-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTV9-MENSAGEM
               MOVE SPACES             TO GFCTV9-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  GFCTRK-COD-RETORNO      NOT EQUAL ZEROS
               MOVE ZEROS              TO GFCTRK-CMUN-RURAL
               MOVE SPACES             TO GFCTRK-CSGL-UF
           END-IF.

      *----------------------------------------------------------------*
       2438-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GP MUNICIPIO. *
      ******************************************************************
      *----------------------------------------------------------------*
       2439-ACESSAR-GFCTB0C9           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF                                OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0C9.
           MOVE WRK-PROX-AGRUPAMENTO
                                       TO CAGPTO-CTA        OF GFCTB0C9.
           MOVE DPROCM-ATUAL                                OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C9.
           MOVE GFCTRK-CMUN-RURAL      TO CMUN-IBGE         OF GFCTB0C9.

           EXEC SQL
             SELECT
                   CSEQ_AGPTO_CTA,
                   DINIC_PRMSS_PCOTE
             INTO
                   :GFCTB0C9.CSEQ-AGPTO-CTA,
                   :GFCTB0C9.DINIC-PRMSS-PCOTE
             FROM   DB2PRD.PRMSS_GRP_MUN
             WHERE
                   CSERVC_TARIF       = :GFCTB0C9.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0C9.CAGPTO-CTA        AND
                   CMUN_IBGE          = :GFCTB0C9.CMUN-IBGE         AND
                   DINIC_PRMSS_PCOTE  IN
                (SELECT MAX(DINIC_PRMSS_PCOTE)
                 FROM   DB2PRD.PRMSS_GRP_MUN
                 WHERE
                   CSERVC_TARIF       = :GFCTB0C9.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0C9.CAGPTO-CTA        AND
                   CMUN_IBGE          = :GFCTB0C9.CMUN-IBGE         AND
                   DINIC_PRMSS_PCOTE <= :GFCTB0C9.DINIC-PRMSS-PCOTE)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               IF GFCTV9-AMBIENTE EQUAL 'O'
                   MOVE 1334               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               ELSE
                   MOVE 1196               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 31                 TO GFCTV9-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTV9-MENSAGEM
               MOVE SQLCA              TO GFCTV9-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2460-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                         WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA    OF GFCTB0C9 TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0C9 TO WRK-DT-INICIO
                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2439-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO UF.     *
      ******************************************************************
      *----------------------------------------------------------------*
       2440-ACESSAR-GFCTB0D3           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF                                OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D3.
           MOVE WRK-PROX-AGRUPAMENTO
                                       TO CAGPTO-CTA        OF GFCTB0D3.
           MOVE DPROCM-ATUAL                                OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0D3.
           MOVE GFCTRK-CSGL-UF         TO CSGL-UF           OF GFCTB0D3.

           EXEC SQL
             SELECT
                   CSEQ_AGPTO_CTA,
                   DINIC_PRMSS_PCOTE
             INTO
                   :GFCTB0D3.CSEQ-AGPTO-CTA,
                   :GFCTB0D3.DINIC-PRMSS-PCOTE
             FROM   DB2PRD.PRMSS_GRP_UF
             WHERE
                   CSERVC_TARIF       = :GFCTB0D3.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0D3.CAGPTO-CTA        AND
                   CSGL_UF            = :GFCTB0D3.CSGL-UF           AND
                   DINIC_PRMSS_PCOTE  IN
                (SELECT MAX(DINIC_PRMSS_PCOTE)
                 FROM   DB2PRD.PRMSS_GRP_UF
                 WHERE
                   CSERVC_TARIF       = :GFCTB0D3.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0D3.CAGPTO-CTA        AND
                   CSGL_UF            = :GFCTB0D3.CSGL-UF           AND
                   DINIC_PRMSS_PCOTE <= :GFCTB0D3.DINIC-PRMSS-PCOTE)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               IF GFCTV9-AMBIENTE EQUAL 'O'
                   MOVE 1335               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               ELSE
                   MOVE 1197               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 32                 TO GFCTV9-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTV9-MENSAGEM
               MOVE SQLCA              TO GFCTV9-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2460-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                         WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA    OF GFCTB0D3 TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0D3 TO WRK-DT-INICIO
                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2440-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER DADOS DA PERMISSAO DE ADESAO AO PACOTE.                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2450-OBTER-DADOS-GFCTB0C6       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0C6.
           MOVE WRK-SEQ-AGPTO          TO CSEQ-AGPTO-CTA    OF GFCTB0C6.
           MOVE WRK-DT-INICIO          TO DINIC-PRMSS-PCOTE OF GFCTB0C6.

           EXEC SQL
             SELECT
                   CSEQ_AGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CINDCD_AGPTO_TOT
             INTO
                   :GFCTB0C6.CSEQ-AGPTO-CTA,
                   :GFCTB0C6.DINIC-PRMSS-PCOTE,
                   :GFCTB0C6.CINDCD-AGPTO-TOT
             FROM  DB2PRD.PRMSS_ADSAO_PCOTE
             WHERE
                   CSERVC_TARIF        = :GFCTB0C6.CSERVC-TARIF
               AND CAGPTO_CTA          = :GFCTB0C6.CAGPTO-CTA
               AND CSEQ_AGPTO_CTA      = :GFCTB0C6.CSEQ-AGPTO-CTA
               AND DINIC_PRMSS_PCOTE   = :GFCTB0C6.DINIC-PRMSS-PCOTE
              AND (CINDCD_ADSAO_INDVD  = :WRK-CINDCD-ADSAO-INDVD OR
                   CINDCD_ADSAO_INDVD  = :WRK-CINDCD-ADSAO-AMBAS)
               AND CIDTFD_PRMSS_INDVD  IN ( 0, 2 )
               AND DFIM_PRMSS_PCOTE    > :GFCTB0C6.DFIM-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -811) OR
              (SQLWARN0                EQUAL 'W'      )
               IF GFCTV9-AMBIENTE EQUAL 'O'
                   MOVE 1336               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               ELSE
                   MOVE 1198               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 33                 TO GFCTV9-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTV9-MENSAGEM
               MOVE SQLCA              TO GFCTV9-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS OR -811
               MOVE 'S'                TO WRK-AGRUPAMENTO-VALIDO
           END-IF.

      *----------------------------------------------------------------*
       2450-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CALCULAR A DATA DE INICIO DA ADESAO INDIVIDUAL                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2460-CALCULAR-DATA-INI          SECTION.
      *----------------------------------------------------------------*

           MOVE DPROCM-ATUAL OF GFCTB0A1(1:2) TO WRK-DIA-PROCM-ATUAL.
           MOVE DPROCM-ATUAL OF GFCTB0A1(4:2) TO WRK-MES-PROCM-ATUAL.
           MOVE DPROCM-ATUAL OF GFCTB0A1(7:4) TO WRK-ANO-PROCM-ATUAL.
           MOVE DINIC-PRMSS-PCOTE OF GFCTB0C6(1:2)
                                            TO WRK-DIA-INI-PRMSS-PCOTE.
           MOVE DINIC-PRMSS-PCOTE OF GFCTB0C6(4:2)
                                            TO WRK-MES-INI-PRMSS-PCOTE.
           MOVE DINIC-PRMSS-PCOTE OF GFCTB0C6(7:4)
                                            TO WRK-ANO-INI-PRMSS-PCOTE.

           IF  WRK-DT-INI-PRMSS-PCOTE       GREATER WRK-DT-PROCM-ATUAL
               MOVE WRK-DT-INI-PRMSS-PCOTE  TO WRK-DT-INI-ADSAO-INDVD
               IF  WRK-DIA-INI-ADSAO-INDVD  NOT EQUAL 01
                   MOVE 01                  TO WRK-DIA-INI-ADSAO-INDVD
                   ADD 1                    TO WRK-MES-INI-ADSAO-INDVD
                   IF  WRK-MES-INI-ADSAO-INDVD
                                            GREATER 12
                       MOVE 01              TO WRK-MES-INI-ADSAO-INDVD
                       ADD 1                TO WRK-ANO-INI-ADSAO-INDVD
                   END-IF
               END-IF
           ELSE
               MOVE WRK-DT-PROCM-ATUAL      TO WRK-DT-INI-ADSAO-INDVD
               MOVE 01                      TO WRK-DIA-INI-ADSAO-INDVD
               ADD 1                        TO WRK-MES-INI-ADSAO-INDVD
               IF  WRK-MES-INI-ADSAO-INDVD  GREATER 12
                   MOVE 01                  TO WRK-MES-INI-ADSAO-INDVD
                   ADD 1                    TO WRK-ANO-INI-ADSAO-INDVD
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2460-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * MOVIMENTAR AS INFORMACOES PARA AREA DE SAIDA.                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2470-MONTAR-OCORRENCIAS         SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO IND-1.

           IF  IND-1                   GREATER 5
               MOVE 'S'                TO WRK-FIM-CURSOR-1
               MOVE CSERVC-TARIF       OF GFCTB0C6
                                       TO GFCTV9-PONTEIRO

               IF GFCTV9-AMBIENTE EQUAL 'O'
                   MOVE 1338               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               ELSE
                   MOVE 1200               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 35                 TO GFCTV9-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTV9-MENSAGEM
               MOVE ZEROS              TO RETURN-CODE
               GO TO 2470-99-FIM
           END-IF.

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO GFCTV9-TARIFA(IND-1).
           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO GFCTV9-NOME-TARIFA(IND-1).
           MOVE WRK-DIA-INI-ADSAO-INDVD
                                TO GFCTV9-DATA-INICIO(IND-1)(1:2).
           MOVE WRK-MES-INI-ADSAO-INDVD
                                TO GFCTV9-DATA-INICIO(IND-1)(4:2).
           MOVE WRK-ANO-INI-ADSAO-INDVD
                                TO GFCTV9-DATA-INICIO(IND-1)(7:4).
           MOVE '.'             TO GFCTV9-DATA-INICIO(IND-1)(3:1)
                                   GFCTV9-DATA-INICIO(IND-1)(6:1).
           MOVE WRK-DIA-FIM-ADSAO-INDVD
                                TO GFCTV9-DATA-FIM(IND-1)(1:2).
           MOVE WRK-MES-FIM-ADSAO-INDVD
                                TO GFCTV9-DATA-FIM(IND-1)(4:2).
           MOVE WRK-ANO-FIM-ADSAO-INDVD
                                TO GFCTV9-DATA-FIM(IND-1)(7:4).
           MOVE '.'             TO GFCTV9-DATA-FIM(IND-1)(3:1)
                                   GFCTV9-DATA-FIM(IND-1)(6:1).

           PERFORM 2310-ACESSAR-GFCTB0A2.
           MOVE WRK-CESTA-CELULAR TO GFCTV9-CESTA-CELULAR(IND-1).


           PERFORM 2300-LER-CURSOR-1.

      *----------------------------------------------------------------*
       2470-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2500-FECHAR-CURSOR-1            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-GFCTB0C6
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               IF GFCTV9-AMBIENTE EQUAL 'O'
                   MOVE 1313               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               ELSE
                   MOVE 1174               TO GFCTG2-COD-MSG
                                              GFCTV9-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 10                 TO GFCTV9-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTV9-MENSAGEM
               MOVE SQLCA              TO GFCTV9-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-VALIDAR-CONTA              SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTV9-AGENCIA  TO LKW-CJUNC-DEPC.
           MOVE GFCTV9-CONTA    TO LKW-CTA-CORR.

           MOVE 'SCCE9100'             TO WRK-MODULO.

           CALL WRK-MODULO       USING LKW-AREA.


           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 22                 TO GFCTV9-COD-RETORNO
               MOVE 1392               TO GFCTV9-COD-MENSAGEM
               MOVE WRK-MSG-SCCE9100   TO GFCTV9-MENSAGEM
               MOVE SPACES             TO GFCTV9-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  LKW-CD-RETORNO          EQUAL 99
               MOVE 18                 TO GFCTV9-COD-RETORNO
               MOVE 1392               TO GFCTV9-COD-MENSAGEM
               MOVE WRK-MSG-SCCE9100   TO GFCTV9-MENSAGEM
               MOVE SPACES             TO GFCTV9-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  LKW-CD-RETORNO          NOT EQUAL ZEROS
               MOVE 18                 TO GFCTV9-COD-RETORNO
               MOVE 1392               TO GFCTV9-COD-MENSAGEM
               MOVE WRK-MSG-SCCE9100   TO GFCTV9-MENSAGEM
               MOVE SPACES             TO GFCTV9-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3200-CONSISTE-AG-CTA            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTV9-AGENCIA  TO CJUNC-DEPDC      OF CLIEV007.
           MOVE GFCTV9-CONTA    TO CCTA-CLI         OF CLIEV007.

           EXEC SQL
             SELECT
                   CID_CLI
             INTO
                   :CLIEV007.CID-CLI
             FROM   DB2PRD.V01CTA_POUPANCA
             WHERE
                   CJUNC_DEPDC        = :CLIEV007.CJUNC-DEPDC AND
                   CCTA_CLI           = :CLIEV007.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               IF GFCTV9-AMBIENTE EQUAL 'O'
                   MOVE '1314'              TO GFCTG2-COD-MSG
                   MOVE  1314               TO GFCTV9-COD-MENSAGEM
               ELSE
                   MOVE '1175'              TO GFCTG2-COD-MSG
                   MOVE  1175               TO GFCTV9-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 11                 TO GFCTV9-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTV9-MENSAGEM
               MOVE SQLCA              TO GFCTV9-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 3210-ACESSAR-CLIEV008
           ELSE
               MOVE 2                  TO WRK-TIPO-CONTA-CLIENTE
               MOVE CID-CLI            OF CLIEV007
                                       TO CID-CLI          OF CLIEV004
               PERFORM 3220-ACESSAR-CLIEV004
           END-IF.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSAR CLIEV008                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       3210-ACESSAR-CLIEV008           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTV9-AGENCIA  TO CJUNC-DEPDC      OF CLIEV008.
           MOVE GFCTV9-CONTA    TO CCTA-CLI         OF CLIEV008.

           EXEC SQL
             SELECT
                   CID_CLI
             INTO
                   :CLIEV008.CID-CLI
             FROM   DB2PRD.V01CTA_CORRENTE
             WHERE
                   CJUNC_DEPDC         = :CLIEV008.CJUNC-DEPDC AND
                   CCTA_CLI            = :CLIEV008.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               IF GFCTV9-AMBIENTE EQUAL 'O'
                  MOVE '1315'               TO GFCTG2-COD-MSG
                  MOVE  1315                TO GFCTV9-COD-MENSAGEM
                ELSE
                   MOVE '1176'              TO GFCTG2-COD-MSG
                   MOVE  1176               TO GFCTV9-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 12                 TO GFCTV9-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTV9-MENSAGEM
               MOVE SQLCA              TO GFCTV9-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               IF GFCTV9-AMBIENTE EQUAL 'O'
                  MOVE '1322'               TO GFCTG2-COD-MSG
                  MOVE  1322                TO GFCTV9-COD-MENSAGEM
               ELSE
                  MOVE '1182'               TO GFCTG2-COD-MSG
                  MOVE  1182                TO GFCTV9-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 19                 TO GFCTV9-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTV9-MENSAGEM
               MOVE SQLCA              TO GFCTV9-SQLCA
               MOVE 04                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           ELSE
               MOVE 1                  TO WRK-TIPO-CONTA-CLIENTE
               MOVE CID-CLI            OF CLIEV008
                                       TO CID-CLI          OF CLIEV004
               PERFORM 3220-ACESSAR-CLIEV004
           END-IF.

      *----------------------------------------------------------------*
       3210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSAR CLIEV004 PARA OBTER DADOS DO CLIENTE                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3220-ACESSAR-CLIEV004           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
ST25X6               CCGC_CPF_ST,
ST25X6               CFLIAL_CGC_ST,
ST25X6               CCTRL_CPF_CGC_ST,
                   CTPO_PSSOA,
                   IPSSOA_COPLT
             INTO
ST25X6              :CLIEV004.CCGC-CPF-ST,
ST25X6              :CLIEV004.CFLIAL-CGC-ST,
ST25X6              :CLIEV004.CCTRL-CPF-CGC-ST,
                   :CLIEV004.CTPO-PSSOA,
                   :CLIEV004.IPSSOA-COPLT
             FROM   DB2PRD.V01CLIENTE_AGENCIA
             WHERE
                   CID_CLI            = :CLIEV004.CID-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               IF GFCTV9-AMBIENTE EQUAL 'O'
                  MOVE '1316'             TO GFCTG2-COD-MSG
                  MOVE  1316              TO  GFCTV9-COD-MENSAGEM
               ELSE
                   MOVE '1177'             TO GFCTG2-COD-MSG
                   MOVE 1177               TO GFCTV9-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 13                 TO GFCTV9-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTV9-MENSAGEM
               MOVE SQLCA              TO GFCTV9-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               IF GFCTV9-AMBIENTE EQUAL 'O'
                   MOVE '1323'               TO GFCTG2-COD-MSG
                   MOVE  1323                TO GFCTV9-COD-MENSAGEM
               ELSE
                   MOVE '1183'               TO GFCTG2-COD-MSG
                   MOVE  1183                TO GFCTV9-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 20                 TO GFCTV9-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTV9-MENSAGEM
               MOVE SQLCA              TO GFCTV9-SQLCA
               MOVE 04                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           ELSE
ST25X6         IF  CFLIAL-CGC-ST OF GFCTB0I0
ST25X6                                 EQUAL ZEROS
ST25X6         OR  CFLIAL-CGC-ST OF GFCTB0I0
ST25X6                                 EQUAL SPACES
ST25X6         OR  CFLIAL-CGC-ST OF GFCTB0I0
ST25X6                                 EQUAL LOW-VALUES
ST25X6            MOVE 'F'             TO WRK-TIPO-PESSOA-CLIENTE
ST25X6         ELSE
ST25X6            MOVE 'J'             TO WRK-TIPO-PESSOA-CLIENTE
ST25X6         END-IF

           END-IF.

      *----------------------------------------------------------------*
       3220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA FINALIZACAO DO PROGRAMA.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
