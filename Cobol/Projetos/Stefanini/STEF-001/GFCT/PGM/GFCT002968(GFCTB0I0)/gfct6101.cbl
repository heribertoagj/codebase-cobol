      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT6101.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      ******************************************************************
      *.  A L E R T A     A L E R T A     A L E R T A     A L E R T A  *
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
      *    PROGRAMA....:   GFCT6101                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - PROCWORK/GP50 *
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - PROCWORK/GP50 *
      *    DATA........:   29/02/2008                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   MODULO PARA INCLUSAO DE ADESAO INDIVIDUAL   *
      *                    DE PACOTES VIGENTES.                        *
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
      *                   DB2PRD.ADSAO_INDVD_PCOTE   GFCTB009          *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTVH - ENTRADA E SAIDA - INC MODULO CORPORATIVO GFCT6101*
      *    I#GFCTG2 - ENTRADA - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTG3 - SAIDA   - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTJM - ENTRADA E SAIDA - OBTER COD SEG GESTAO TARIFA.   *
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

231109*----------------------------------------------------------------*
231109*           SONDA PROCWORK -  CONSULTORIA - ALTERACAO            *
231109*----------------------------------------------------------------*
231109*    ANALISTA....:   MARCUS VINICIUS PINHEIRO CURTO /   GRUPO 50 *
231109*    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA    /   GRUPO 50 *
231109*    DATA........:   23/11/2009                                  *
231109*    OBJETIVO....:   TRATAR AS TARIFAS 1123 E 1124 NO PROCESSO DE*
231109*                    ADESAO.                                     *
231109*----------------------------------------------------------------*

ABR10 *----------------------------------------------------------------*
ABR10 *           SONDA PROCWORK -  CONSULTORIA - ALTERACAO            *
ABR10 *----------------------------------------------------------------*
ABR10 *    ANALISTA....:   MARCUS VINICIUS PINHEIRO CURTO /   GRUPO 50 *
ABR10 *    PROGRAMADOR.:   FABRICA SOFTWARE               /   GRUPO 50 *
ABR10 *    DATA........:   ABR/2010                                    *
ABR10 *    OBJETIVO....:   TRATAR AS TARIFAS CESTA CELULAR NO PROCESSO *
ABR10 *                    DE ADESAO.                                  *
ABR10 *----------------------------------------------------------------*
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
      *----------------------------------------------------------------*
      *                 U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
      *        SONDA IT       - CONSULTORIA - ALTERACAO - BI1113       *
      *----------------------------------------------------------------*
      *                                                                *
      *    ANALISTA....:  UBIRAJARA          - SONDA IT                *
      *    DATA........:  NOV/2013                                     *
      *    OBJETIVO....:  INCLUIR TRATAMENTO DE TIPO DE CONTA = 3      *
      *                   ( CONTA BENEFICIO ).                         *
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
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-TIPO-PESSOA-CLIENTE     PIC  X(001)         VALUE SPACES.
       77  WRK-TIPO-CONTA-CLIENTE      PIC S9(001) COMP-3  VALUE ZEROS.
______*77__WRK-NOME-TABELA_____________PIC__X(018)_________VALUE_SPACES.
______*77__WRK-COD-TABELA______________PIC__X(008)_________VALUE_SPACES.
______*77__WRK-COMANDO-SQL_____________PIC__X(010)_________VALUE_SPACES.
______*77__WRK-LOCAL-ERRO______________PIC__X(004)_________VALUE_SPACES.
JUN10  77  WRK-RESTO                   PIC  9(003) COMP-3  VALUE ZEROS.
JUN10  77  WRK-INTEIRO                 PIC  9(004) COMP-3  VALUE ZEROS.
______*77__WRK-FIM-CURSOR-1____________PIC__X(001)_________VALUE_SPACES.
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
       77  WRK-FIM-CURSOR              PIC  X(001)         VALUE 'N'.
       77  WRK-EXISTE-COMPONENTE       PIC  X(001)         VALUE 'N'.
       77  WRK-MES-PK                  PIC  9(002) COMP-3  VALUE ZERO.
       77  WRK-ANO-PK                  PIC  9(004) COMP-3  VALUE ZERO.

       01  WRK-DT-PROCM-ATUAL          PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-PROCM-ATUAL-R        REDEFINES WRK-DT-PROCM-ATUAL.
           03  WRK-ANO-PROCM-ATUAL     PIC  X(004).
           03  WRK-MES-PROCM-ATUAL     PIC  X(002).
           03  WRK-DIA-PROCM-ATUAL     PIC  X(002).

       01  WRK-DT-INI-PRMSS-PCOTE      PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-INI-PRMSS-PCOTE-R    REDEFINES WRK-DT-INI-PRMSS-PCOTE.
           03  WRK-ANO-INI-PRMSS-PCOTE PIC  X(004).
           03  WRK-MES-INI-PRMSS-PCOTE PIC  X(002).
           03  WRK-DIA-INI-PRMSS-PCOTE PIC  X(002).

       01  WRK-DT-INI-ADSAO-INDVD      PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-INI-ADSAO-INDVD-R    REDEFINES WRK-DT-INI-ADSAO-INDVD.
           03  WRK-AAAAMM-INI-ADSAO-INDVD
                                       PIC  9(006).
           03  FILLER                  REDEFINES
                                             WRK-AAAAMM-INI-ADSAO-INDVD.
               05  WRK-ANO-INI-ADSAO-INDVD
                                       PIC  X(004).
               05  WRK-MES-INI-ADSAO-INDVD
                                       PIC  X(002).
           03  FILLER                  REDEFINES
                                             WRK-AAAAMM-INI-ADSAO-INDVD.
               05  WRK-ANO-INI-ADSAO-INDVD-R
                                       PIC  9(004).
               05  WRK-MES-INI-ADSAO-INDVD-R
                                       PIC  9(002).
           03  WRK-DIA-INI-ADSAO-INDVD.
               05  WRK-DIA-INI-ADSAO-INDVD-R PIC  9(002).

       01  WRK-DT-FIM-ADSAO-INDVD      PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-FIM-ADSAO-INDVD-R    REDEFINES WRK-DT-FIM-ADSAO-INDVD.
           03  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       PIC  9(006).
           03  FILLER                  REDEFINES
                                             WRK-AAAAMM-FIM-ADSAO-INDVD.
               05  WRK-ANO-FIM-ADSAO-INDVD
                                       PIC  X(004).
               05  WRK-MES-FIM-ADSAO-INDVD
                                       PIC  X(002).
           03  FILLER                  REDEFINES
                                             WRK-AAAAMM-FIM-ADSAO-INDVD.
               05  WRK-ANO-FIM-ADSAO-INDVD-R
                                       PIC  9(004).
               05  WRK-MES-FIM-ADSAO-INDVD-R
                                       PIC  9(002).
           03  WRK-DIA-FIM-ADSAO-INDVD.
               05  WRK-DIA-FIM-ADSAO-INDVD-R PIC  9(002).

______*01__WRK-MSG001.
______*____05_FILLER___________________PIC__X(026)_________VALUE
______*_______'ERRO_NA_CHAMADA_AO_MODULO_'.
______*____05_WRK-MODULO-MSG___________PIC__X(008)_________VALUE_SPACES.

______*01__WRK-MSG002.
______*____05_FILLER___________________PIC__X(030)_________VALUE
______*_______'ERRO_NOS_PARAMETROS_DO_MODULO'.
______*____05_WRK-MODULO-MSG-2_________PIC__X(008)_________VALUE_SPACES.

______*01__WRK-MSG003.
______*____05_FILLER___________________PIC__X(040)_________VALUE
______*_______'INCONSISTENCIA_NOS_PARAMETROS_DO_MODULO'.
______*____05_WRK-MODULO-MSG-3_________PIC__X(008)_________VALUE_SPACES.

       01  WRK-MSG-SCCE9100.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA AO MODULO '.

ABR10  01  WRK-CESTA-CELULAR           PIC  X(001)         VALUE SPACES.

       01  WRK-CAMPO-9                 PIC  9(009)         VALUE ZEROS.
       01  FILLER REDEFINES            WRK-CAMPO-9.
           05 FILLER                   PIC  X(002).
           05 WRK-CAMPO-9-7            PIC  9(007).

       01  WRK-CAMPO-S7                PIC +9(007)         VALUE
                                          '+0000000'.
       01  FILLER                      REDEFINES           WRK-CAMPO-S7.
           05  FILLER                  PIC  X(001).
           05  WRK-CAMPO-S7-7          PIC  9(007).


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5527 *'.
      *----------------------------------------------------------------*

       01  WRK-5527-AREA-ENTRADA.
           05  WRK-5527-AMBIENTE           PIC  X(001) VALUE SPACE.
           05  WRK-5527-CHAMADOR           PIC  X(008) VALUE SPACE.
           05  WRK-5527-AGENCIA-CLIENTE    PIC  9(005) VALUE ZERO.
           05  WRK-5527-CONTA-CLIENTE      PIC  9(013) VALUE ZERO.

       01  WRK-5527-AREA-SAIDA.
           05  WRK-5527-COD-RETORNO        PIC  9(002) VALUE ZERO.
           05  WRK-5527-COD-SQL-ERRO       PIC S9(003) VALUE ZERO.
           05  WRK-5527-COD-MSG-GFCT       PIC  9(004) VALUE ZERO.
           05  WRK-5527-DESC-MSG           PIC  X(070) VALUE SPACE.
           05  WRK-5527-QTD-OCORR          PIC  9(005) VALUE ZERO.
           05  WRK-5527-DADOS-RETORNO.
ST25X6*      10  WRK-5527-CCGC-CPF         PIC  9(009) VALUE ZERO.
ST25X6       10  WRK-5527-CCGC-CPF         PIC  X(009) VALUE SPACES.
ST25X6*      10  WRK-5527-CFLIAL-CGC       PIC  9(004) VALUE ZERO.
ST25X6       10  WRK-5527-CFLIAL-CGC       PIC  X(004) VALUE SPACES.
             10  WRK-5527-CCTRL-CPF-CGC    PIC  9(002) VALUE ZERO.
             10  WRK-5527-CPOSTO-SERVC     PIC  9(003) VALUE ZERO.
             10  WRK-5527-CSGMTO-CLI       PIC  9(003) VALUE ZERO.

       01  WRK-IO-PCB.
           03  WRK-IO-TERM              PIC  X(008) VALUE SPACE.
           03  FILLER                   PIC  X(002) VALUE SPACE.
           03  WRK-IO-STA               PIC  X(002) VALUE SPACE.
           03  FILLER                   PIC  X(012) VALUE SPACE.
           03  WRK-IO-MODNAME           PIC  X(008) VALUE SPACE.

       01  WRK-ALT-PCB.
           03  WRK-ALT-PCBNAME          PIC  X(008) VALUE SPACE.
           03  FILLER                   PIC  X(002) VALUE SPACE.
           03  WRK-ALT-STATUS           PIC  X(002) VALUE SPACE.
           03  FILLER                   PIC  X(012) VALUE SPACE.
           03  WRK-ALT-MODNAME          PIC  X(008) VALUE SPACE.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA PARA MENSAGENS MODULO GFCT5523 - MENSAGENS *'.
      *----------------------------------------------------------------*

       01  WRK-MSG-1                    PIC  X(070) VALUE
           'ADESAO AO PACOTE - NAO FOI POSSIVEL OBTER A MSG NO MODULO
      -    'GFCT5523'.

       01  WRK-MSG-2                    PIC  X(070) VALUE
           'ADESAO AO PACOTE - GFCTG3-ERRO IGUAL A 9'.

       01  WRK-MSG-3                    PIC  X(070) VALUE
           'ADESAO AO PACOTE - GFCTG3-ERRO IGUAL A 1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TRATAMENTO DE ERRO *'.
      *----------------------------------------------------------------*

______*COPY_'I#BRAD7C'.

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

______*____EXEC_SQL
______*______INCLUDE_GFCTB0C8
______*____END-EXEC.

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
             INCLUDE GFCTB0D8
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
             INCLUDE GFCTB009
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB029
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DECLARACAO DE CURSOR *'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR01-GFCTB029 CURSOR FOR
             SELECT DISTINCT(CTARIF_COMPO_PCOTE)
             FROM  DB2PRD.COMPS_TARIF_PCOTE
             WHERE
                   CTARIF_PCOTE       = :GFCTB029.CTARIF-PCOTE
             ORDER BY CTARIF_COMPO_PCOTE
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTVH'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTVH-REG.
      *================================================================*

      ******************************************************************
      * ROTINA PRINCIPAL.                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           IF GFCTVH-AMBIENTE EQUAL 'B' OR 'b'
               CALL 'POOL1050'
           END-IF.

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR-REGISTROS.

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

           MOVE SPACES                 TO GFCTVH-SAIDA.

           INITIALIZE GFCTVH-SAIDA.

           PERFORM 1100-CONSISTIR-DADOS.
           PERFORM 1300-OBTER-DATA-PROCESSAMENTO.
           PERFORM 1400-CONSISTE-PACOTE.
           PERFORM 1150-VALIDAR-CONTA.
           PERFORM 1200-CONSISTE-AG-CTA.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONSISTENCIA DOS DADOS DE ENTRADA.                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF  GFCTVH-AMBIENTE         EQUAL 'o'
               MOVE 'O'                TO GFCTVH-AMBIENTE
           END-IF.

           IF  GFCTVH-AMBIENTE         EQUAL 'b'
               MOVE 'B'                TO GFCTVH-AMBIENTE
           END-IF.

           IF (GFCTVH-AGENCIA           NOT NUMERIC)                OR
              (GFCTVH-AGENCIA           EQUAL ZEROS)                OR
              (GFCTVH-CONTA             NOT NUMERIC)                OR
              (GFCTVH-CONTA             EQUAL ZEROS)                OR
              (GFCTVH-PACOTE            NOT NUMERIC)                OR
              (GFCTVH-AMBIENTE          NOT EQUAL 'O' AND 'B')
               MOVE 'B'                 TO GFCTVH-AMBIENTE
                   MOVE '1212'             TO GFCTG2-COD-MSG
                   MOVE 1212               TO GFCTVH-COD-MENSAGEM
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                   TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG     TO GFCTVH-MENSAGEM
               MOVE SPACES              TO GFCTVH-SQLCA
               MOVE 12                  TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

231109*****IF__GFCTVH-PACOTE____________EQUAL_01123_OR_01124
231109*********IF_GFCTVH-AMBIENTE_EQUAL_'B'
231109*************MOVE_'1482'_____________TO_GFCTG2-COD-MSG
231109*************MOVE_1482_______________TO_GFCTVH-COD-MENSAGEM
231109*********ELSE
231109*************MOVE_'1626'_____________TO_GFCTG2-COD-MSG
231109*************MOVE_1626_______________TO_GFCTVH-COD-MENSAGEM
231109*********END-IF
231109*********PERFORM_1500-OBTER-DESC-MSG
231109*********MOVE_49__________________TO_GFCTVH-COD-RETORNO
231109*********MOVE_GFCTG3-DESC-MSG_____TO_GFCTVH-MENSAGEM
231109*********MOVE_SPACES______________TO_GFCTVH-SQLCA
231109*********MOVE_04__________________TO_RETURN-CODE
231109*********PERFORM_4000-FINALIZAR
231109*****END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1150-VALIDAR-CONTA              SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVH-AGENCIA  TO LKW-CJUNC-DEPC.
           MOVE GFCTVH-CONTA    TO LKW-CTA-CORR.

           MOVE 'SCCE9100'             TO WRK-MODULO.

           CALL WRK-MODULO                USING
                                             LKW-AREA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 22                 TO GFCTVH-COD-RETORNO
                   MOVE 1392               TO GFCTVH-COD-MENSAGEM
               MOVE WRK-MSG-SCCE9100   TO GFCTVH-MENSAGEM
               MOVE SPACES             TO GFCTVH-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  LKW-CD-RETORNO          EQUAL 99
               MOVE 18                 TO GFCTVH-COD-RETORNO
                   MOVE 1392               TO GFCTVH-COD-MENSAGEM
               MOVE WRK-MSG-SCCE9100   TO GFCTVH-MENSAGEM
               MOVE SPACES             TO GFCTVH-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  LKW-CD-RETORNO          NOT EQUAL ZEROS
               MOVE 03                 TO GFCTVH-COD-RETORNO
                   MOVE 1306               TO GFCTVH-COD-MENSAGEM
               MOVE LKW-MENSAGEM       TO GFCTVH-MENSAGEM
               MOVE SPACES             TO GFCTVH-SQLCA
               MOVE 04                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-CONSISTE-AG-CTA            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVH-AGENCIA  TO CJUNC-DEPDC      OF CLIEV007.
           MOVE GFCTVH-CONTA    TO CCTA-CLI         OF CLIEV007.

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
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1314'             TO GFCTG2-COD-MSG
                   MOVE 1314               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1175'             TO GFCTG2-COD-MSG
                   MOVE 1175               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 11                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SQLCA(1:136)             TO GFCTVH-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 1210-ACESSAR-CLIEV008
           ELSE
               MOVE 2                  TO WRK-TIPO-CONTA-CLIENTE
               MOVE CID-CLI            OF CLIEV007
                                       TO CID-CLI          OF CLIEV004
               PERFORM 1220-ACESSAR-CLIEV004
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSAR CLIEV008                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       1210-ACESSAR-CLIEV008           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVH-AGENCIA  TO CJUNC-DEPDC      OF CLIEV008.
           MOVE GFCTVH-CONTA    TO CCTA-CLI         OF CLIEV008.

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
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1315'             TO GFCTG2-COD-MSG
                   MOVE 1315               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1176'             TO GFCTG2-COD-MSG
                   MOVE 1176               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 12                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SQLCA(1:136)             TO GFCTVH-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1322'             TO GFCTG2-COD-MSG
                   MOVE 1322               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1182'             TO GFCTG2-COD-MSG
                   MOVE 1182               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 19                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SQLCA(1:136)             TO GFCTVH-SQLCA
               MOVE 04                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           ELSE
               MOVE 1                  TO WRK-TIPO-CONTA-CLIENTE
               MOVE CID-CLI            OF CLIEV008
                                       TO CID-CLI          OF CLIEV004
               PERFORM 1220-ACESSAR-CLIEV004
           END-IF.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSAR CLIEV004 PARA OBTER DADOS DO CLIENTE                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1220-ACESSAR-CLIEV004           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                   CCGC_CPF,
                   CFLIAL_CGC,
                   CCTRL_CPF_CGC,
ST25X6             CCGC_CPF_ST,
ST25X6             CFLIAL_CGC_ST,
ST25X6             CCTRL_CPF_CGC_ST,
                   CTPO_PSSOA,
                   IPSSOA_COPLT
             INTO
                   :CLIEV004.CCGC-CPF,
                   :CLIEV004.CFLIAL-CGC,
                   :CLIEV004.CCTRL-CPF-CGC,
ST25X6             :CLIEV004.CCGC-CPF-ST,
ST25X6             :CLIEV004.CFLIAL-CGC-ST,
ST25X6             :CLIEV004.CCTRL-CPF-CGC-ST,
                   :CLIEV004.CTPO-PSSOA,
                   :CLIEV004.IPSSOA-COPLT
             FROM   DB2PRD.V01CLIENTE_AGENCIA
             WHERE
                   CID_CLI            = :CLIEV004.CID-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1316'             TO GFCTG2-COD-MSG
                   MOVE 1316               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1177'             TO GFCTG2-COD-MSG
                   MOVE 1177               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 13                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SQLCA(1:136)             TO GFCTVH-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1323'             TO GFCTG2-COD-MSG
                   MOVE 1323               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1183'             TO GFCTG2-COD-MSG
                   MOVE 1183               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 20                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SQLCA(1:136)             TO GFCTVH-SQLCA
               MOVE 04                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           ELSE
               IF  CFLIAL-CGC OF CLIEV004
                                       EQUAL ZEROS
ST25X6         OR  CFLIAL-CGC OF CLIEV004
ST25X6                                 EQUAL SPACES
ST25X6         OR  CFLIAL-CGC OF CLIEV004
ST25X6                                 EQUAL LOW-VALUES 
                   MOVE 'F'            TO WRK-TIPO-PESSOA-CLIENTE
               ELSE
                   MOVE 'J'            TO WRK-TIPO-PESSOA-CLIENTE
               END-IF
               PERFORM 3010-OBTER-DADOS-GFCTB0D8
           END-IF.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA OBTER A DATA DE PROCESSAMENTO.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-OBTER-DATA-PROCESSAMENTO   SECTION.
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
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1309'             TO GFCTG2-COD-MSG
                   MOVE 1309               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1170'             TO GFCTG2-COD-MSG
                   MOVE 1170               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 6                  TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SQLCA(1:136)             TO GFCTVH-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1400-CONSISTE-PACOTE           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVH-PACOTE TO CSERVC-TARIF       OF GFCTB0C6.
           MOVE DPROCM-ATUAL  TO DFIM-PRMSS-PCOTE   OF GFCTB0C6.
           MOVE 'I'           TO CINDCD-ADSAO-INDVD OF GFCTB0C6.



           EXEC SQL
             SELECT
                   CSERVC_TARIF
             INTO
                   :GFCTB0C6.CSERVC-TARIF

             FROM  DB2PRD.PRMSS_ADSAO_PCOTE
               WHERE
                   CSERVC_TARIF       = :GFCTB0C6.CSERVC-TARIF     AND
                   DFIM_PRMSS_PCOTE   > :GFCTB0C6.DFIM-PRMSS-PCOTE AND
                   CINDCD_ADSAO_INDVD = :GFCTB0C6.CINDCD-ADSAO-INDVD
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                       AND -811) OR
              (SQLWARN0                EQUAL 'W'      )
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1339'             TO GFCTG2-COD-MSG
                   MOVE 1339               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1202'             TO GFCTG2-COD-MSG
                   MOVE 1202               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 36                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SQLCA(1:136)             TO GFCTVH-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1340'             TO GFCTG2-COD-MSG
                   MOVE 1340               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1203'             TO GFCTG2-COD-MSG
                   MOVE 1203               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 37                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SQLCA(1:136)             TO GFCTVH-SQLCA
               MOVE 04                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

BI0511     PERFORM 1410-ACESSAR-GFCTB0A2
ABR10      IF  WRK-CESTA-CELULAR  EQUAL 'S'
ABR10          IF GFCTVH-AMBIENTE EQUAL 'B'
ABR10              MOVE '1482'             TO GFCTG2-COD-MSG
ABR10              MOVE  1482              TO GFCTVH-COD-MENSAGEM
ABR10          ELSE
ABR10              MOVE '1626'             TO GFCTG2-COD-MSG
ABR10              MOVE  1626              TO GFCTVH-COD-MENSAGEM
ABR10          END-IF
ABR10          PERFORM 1500-OBTER-DESC-MSG
ABR10          MOVE 49                  TO GFCTVH-COD-RETORNO
ABR10          MOVE GFCTG3-DESC-MSG     TO GFCTVH-MENSAGEM
ABR10          MOVE SPACES              TO GFCTVH-SQLCA
ABR10          MOVE 04                  TO RETURN-CODE
ABR10          PERFORM 4000-FINALIZAR
ABR10      END-IF.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

BI0511******************************************************************
BI0511*    ROTINA ACESSAR GFCTB0A2                                     *
BI0511******************************************************************
BI0511*----------------------------------------------------------------*
BI0511 1410-ACESSAR-GFCTB0A2       SECTION.
BI0511*----------------------------------------------------------------*
BI0511
BI0511     MOVE GFCTVH-PACOTE  TO CSERVC-TARIF        OF GFCTB0A2.
BI0511
BI0511     MOVE DPROCM-ATUAL   OF GFCTB0A1
BI0511                         TO DINIC-VGCIA-TARIF   OF GFCTB0A2.
BI0511
BI0511     MOVE DPROCM-ATUAL   OF GFCTB0A1
BI0511                         TO DFIM-VGCIA-TARIF    OF GFCTB0A2.
BI0511
BI0511     EXEC SQL
BI0511          SELECT
BI0511               CINDCD_CREDT_CLULR
BI0511          INTO
BI0511              :GFCTB0A2.CINDCD-CREDT-CLULR
BI0511          FROM   DB2PRD.PARM_SERVC_TARIF
BI0511          WHERE  CSERVC_TARIF       = :GFCTB0A2.CSERVC-TARIF
BI0511            AND  DINIC_VGCIA_TARIF <= :GFCTB0A2.DINIC-VGCIA-TARIF
BI0511            AND  DFIM_VGCIA_TARIF  >= :GFCTB0A2.DFIM-VGCIA-TARIF
BI0511     END-EXEC.
BI0511
BI0511     IF (SQLCODE                NOT EQUAL ZEROS AND +100) OR
BI0511        (SQLWARN0               EQUAL 'W')
BI0511         IF GFCTVH-AMBIENTE EQUAL 'O'
BI0511             MOVE '1318'             TO GFCTG2-COD-MSG
BI0511             MOVE  1318              TO GFCTVH-COD-MENSAGEM
BI0511         ELSE
BI0511             MOVE '1202'             TO GFCTG2-COD-MSG
BI0511             MOVE  1202              TO GFCTVH-COD-MENSAGEM
BI0511         END-IF
BI0511         PERFORM 1500-OBTER-DESC-MSG
BI0511         MOVE 36                 TO GFCTVH-COD-RETORNO
BI0511         MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
BI0511         MOVE SQLCA(1:136)             TO GFCTVH-SQLCA
BI0511         MOVE 12                 TO RETURN-CODE
BI0511         PERFORM 4000-FINALIZAR
BI0511     END-IF.
BI0511
BI0511     IF  CINDCD-CREDT-CLULR OF GFCTB0A2   EQUAL   'S'
BI0511         MOVE 'S'                TO WRK-CESTA-CELULAR
BI0511     ELSE
BI0511         MOVE 'N'                TO WRK-CESTA-CELULAR
BI0511     END-IF.
BI0511
BI0511*----------------------------------------------------------------*
BI0511 1410-99-FIM.                    EXIT.
BI0511*----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA ACESSO AO MODULO DE DESCRICOES DE MENSAGENS.       *
      ******************************************************************
      *----------------------------------------------------------------*
       1500-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE 'GFCT6101'             TO GFCTG2-TRANSACAO
                                          GFCTG2-FUNCAO.
           MOVE '6101'                 TO GFCTG2-FUNC-BDSCO.
           MOVE GFCTVH-AMBIENTE        TO GFCTG2-TIPO-PROC.
           MOVE 'N'                    TO GFCTG2-FIM.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 2                  TO GFCTVH-COD-RETORNO
                   MOVE  1305              TO GFCTVH-COD-MENSAGEM
               MOVE WRK-MSG-1          TO GFCTVH-MENSAGEM
               MOVE 12                 TO RETURN-CODE
               MOVE SPACES             TO GFCTVH-SQLCA
               MOVE 04                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 4                  TO GFCTVH-COD-RETORNO
                   MOVE  1306              TO GFCTVH-COD-MENSAGEM
               MOVE WRK-MSG-2          TO GFCTVH-MENSAGEM
               MOVE SPACES             TO GFCTVH-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE 4                  TO GFCTVH-COD-RETORNO
                   MOVE  1307              TO GFCTVH-COD-MENSAGEM
               MOVE WRK-MSG-3          TO GFCTVH-MENSAGEM
               MOVE SPACES             TO GFCTVH-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      ******************************************************************
      * PROCESSAR DADOS OBTIDOS NO CURSOR 1 DA TABELA GFCTB0C6.        *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR-REGISTROS         SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-TARIFA-VALIDA.

BI1113     IF  (CTPO-CTA-DEB-TARIF OF GFCTB0D8  EQUAL  3)
BI1113     AND (WRK-TIPO-CONTA-CLIENTE          EQUAL  1)
BI1113         MOVE   CTPO-CTA-DEB-TARIF OF GFCTB0D8   TO
BI1113                WRK-TIPO-CONTA-CLIENTE
BI1113     END-IF.

           IF  CTPO-CTA-DEB-TARIF      OF GFCTB0D8
                                       EQUAL WRK-TIPO-CONTA-CLIENTE
               IF  CPSSOA-SERVC-TARIF  OF GFCTB0D8 EQUAL '3'    OR
                  (CPSSOA-SERVC-TARIF  OF GFCTB0D8 EQUAL '1'    AND
                   WRK-TIPO-PESSOA-CLIENTE       EQUAL 'F')   OR
                  (CPSSOA-SERVC-TARIF  OF GFCTB0D8 EQUAL '2'    AND
                   WRK-TIPO-PESSOA-CLIENTE       EQUAL 'J')
                   PERFORM 2200-CALCULAR-DATA-FIM

                   MOVE 014            TO WRK-PROX-AGRUPAMENTO

                   MOVE 'N'            TO WRK-FIM-AGRUPAMENTO

                   MOVE 'N'            TO WRK-AGRUPAMENTO-VALIDO

                   PERFORM 3020-IDENTIFICAR-PERMISSAO UNTIL
                           WRK-FIM-AGRUPAMENTO        EQUAL 'S' OR
                           WRK-AGRUPAMENTO-VALIDO     EQUAL 'S'

                   IF  WRK-AGRUPAMENTO-VALIDO         EQUAL 'S'
                       MOVE 'S'        TO WRK-TARIFA-VALIDA
                   END-IF
               END-IF
           END-IF.

           IF  WRK-TARIFA-VALIDA       EQUAL 'S'
               PERFORM 3150-VER-TARIFA-PACOTE-CAD
               PERFORM 3100-VER-TARIFA-COMP-CAD
               PERFORM 3200-INSERIR-GFCTB009
           ELSE
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1344'             TO GFCTG2-COD-MSG
                   MOVE 1344               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1206'             TO GFCTG2-COD-MSG
                   MOVE 1206               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 41                TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CALCULAR A DATA DE INICIO DA ADESAO INDIVIDUAL                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-CALCULAR-DATA-INI          SECTION.
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
               IF  WRK-DIA-INI-ADSAO-INDVD  NOT EQUAL '01'
                   MOVE '01'                 TO WRK-DIA-INI-ADSAO-INDVD
                   MOVE WRK-MES-INI-ADSAO-INDVD-R TO WRK-MES-PK
                   ADD 1                   TO WRK-MES-PK
                   MOVE WRK-MES-PK         TO WRK-MES-INI-ADSAO-INDVD-R
                   IF  WRK-MES-INI-ADSAO-INDVD
                                            GREATER '12'
                       MOVE '01'             TO WRK-MES-INI-ADSAO-INDVD
                       MOVE WRK-ANO-INI-ADSAO-INDVD-R TO WRK-ANO-PK
                       ADD 1                TO WRK-ANO-PK
                       MOVE WRK-ANO-PK     TO WRK-ANO-INI-ADSAO-INDVD-R
                   END-IF
               END-IF
           ELSE
               MOVE WRK-DT-PROCM-ATUAL      TO WRK-DT-INI-ADSAO-INDVD
               MOVE '01'                     TO WRK-DIA-INI-ADSAO-INDVD
               MOVE WRK-MES-INI-ADSAO-INDVD-R TO WRK-MES-PK
               ADD 1                        TO WRK-MES-PK
               MOVE WRK-MES-PK             TO WRK-MES-INI-ADSAO-INDVD-R
               IF  WRK-MES-INI-ADSAO-INDVD  GREATER '12'
                   MOVE '01'                 TO WRK-MES-INI-ADSAO-INDVD
                   MOVE WRK-ANO-INI-ADSAO-INDVD-R TO WRK-ANO-PK
                   ADD 1                    TO WRK-ANO-PK
                   MOVE WRK-ANO-PK         TO WRK-ANO-INI-ADSAO-INDVD-R
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CALCULAR A DATA DE FIM DA ADESAO INDIVIDUAL.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-CALCULAR-DATA-FIM          SECTION.
      *----------------------------------------------------------------*

           PERFORM 3015-OBTER-DADOS-GFCTB0A2.

           MOVE DFIM-VGCIA-TARIF       OF GFCTB0A2(1:2)
                                       TO WRK-DIA-FIM-ADSAO-INDVD.
           MOVE DFIM-VGCIA-TARIF       OF GFCTB0A2(4:2)
                                       TO WRK-MES-FIM-ADSAO-INDVD.
           MOVE DFIM-VGCIA-TARIF       OF GFCTB0A2(7:4)
                                       TO WRK-ANO-FIM-ADSAO-INDVD.

           IF ((WRK-MES-FIM-ADSAO-INDVD EQUAL '01' OR '03' OR
                                             '05' OR '07' OR
                                             '08' OR '10' OR '12') AND
                WRK-DIA-FIM-ADSAO-INDVD EQUAL '31')                 OR
              ((WRK-MES-FIM-ADSAO-INDVD EQUAL '04' OR '06' OR
                                                '09' OR '11') AND
                  WRK-DIA-FIM-ADSAO-INDVD EQUAL '30')               OR
              (WRK-MES-FIM-ADSAO-INDVD EQUAL '02' AND
                    WRK-DIA-FIM-ADSAO-INDVD EQUAL '29')
               NEXT SENTENCE
           ELSE
               MOVE WRK-ANO-FIM-ADSAO-INDVD-R TO WRK-ANO-PK
               DIVIDE WRK-ANO-PK BY 4
                      GIVING WRK-INTEIRO REMAINDER WRK-RESTO
               IF WRK-MES-FIM-ADSAO-INDVD EQUAL '02' AND
                  WRK-RESTO               NOT EQUAL ZEROS AND
                  WRK-DIA-FIM-ADSAO-INDVD EQUAL '28'
                  NEXT SENTENCE
               ELSE
                  MOVE WRK-MES-FIM-ADSAO-INDVD-R TO WRK-MES-PK
                  SUBTRACT 1          FROM WRK-MES-PK
                  MOVE WRK-MES-PK TO      WRK-MES-FIM-ADSAO-INDVD-R
                  EVALUATE WRK-MES-FIM-ADSAO-INDVD
                     WHEN '00'
                        MOVE '31'        TO WRK-DIA-FIM-ADSAO-INDVD
                        MOVE '12'        TO WRK-MES-FIM-ADSAO-INDVD
                        MOVE WRK-ANO-FIM-ADSAO-INDVD-R TO WRK-ANO-PK
                        SUBTRACT 1     FROM WRK-ANO-PK
                        MOVE WRK-ANO-PK TO WRK-ANO-FIM-ADSAO-INDVD-R
                     WHEN '12'
                        MOVE '31'        TO WRK-DIA-FIM-ADSAO-INDVD
                     WHEN '11'
                        MOVE '30'        TO WRK-DIA-FIM-ADSAO-INDVD
                     WHEN '02'
                        IF WRK-RESTO   EQUAL ZEROS
                           MOVE '28'     TO WRK-DIA-FIM-ADSAO-INDVD
                        ELSE
                           MOVE '29'     TO WRK-DIA-FIM-ADSAO-INDVD
                        END-IF
                  END-EVALUATE
               END-IF
            END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER DADOS DA TARIFA.                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       3010-OBTER-DADOS-GFCTB0D8       SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF     OF GFCTB0D8.

           EXEC SQL
             SELECT
                   RSERVC_TARIF_REDZD,
                   CPSSOA_SERVC_TARIF,
                   CTPO_CTA_DEB_TARIF,
                   CTPO_SERVC_TARIF
             INTO
                   :GFCTB0D8.RSERVC-TARIF-REDZD,
                   :GFCTB0D8.CPSSOA-SERVC-TARIF,
                   :GFCTB0D8.CTPO-CTA-DEB-TARIF,
                   :GFCTB0D8.CTPO-SERVC-TARIF
             FROM   DB2PRD.SERVC_TARIF_PRINC
             WHERE
                   CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1317'             TO GFCTG2-COD-MSG
                   MOVE 1317               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1178'             TO GFCTG2-COD-MSG
                   MOVE 1178               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 14                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SQLCA(1:136)             TO GFCTVH-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF CTPO-SERVC-TARIF OF GFCTB0D8 EQUAL 2
              NEXT SENTENCE
           ELSE
              IF CTPO-SERVC-TARIF OF GFCTB0D8 EQUAL 4
                 MOVE '1926'             TO GFCTG2-COD-MSG
                 MOVE 1926               TO GFCTVH-COD-MENSAGEM
                 PERFORM 1500-OBTER-DESC-MSG
                 MOVE 14                 TO GFCTVH-COD-RETORNO
                 MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
                 MOVE SQLCA(1:136)             TO GFCTVH-SQLCA
                 MOVE 12                 TO RETURN-CODE
                 PERFORM 4000-FINALIZAR
              ELSE
                 MOVE '1925'             TO GFCTG2-COD-MSG
                 MOVE 1925               TO GFCTVH-COD-MENSAGEM
                 PERFORM 1500-OBTER-DESC-MSG
                 MOVE 14                 TO GFCTVH-COD-RETORNO
                 MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
                 MOVE SQLCA(1:136)             TO GFCTVH-SQLCA
                 MOVE 12                 TO RETURN-CODE
                 PERFORM 4000-FINALIZAR
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       3010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER DADOS DO PARAMETRO DA TARIFA.                            *
      ******************************************************************
      *----------------------------------------------------------------*
       3015-OBTER-DADOS-GFCTB0A2       SECTION.
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
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1318'             TO GFCTG2-COD-MSG
                   MOVE 1318               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1179'             TO GFCTG2-COD-MSG
                   MOVE 1179               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 15                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SQLCA(1:136)             TO GFCTVH-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       3015-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * IDENTIFICAR AGRUPAMENTO COM PERMISSAO DE ADESAO AO PACOTE.     *
      ******************************************************************
      *----------------------------------------------------------------*
       3020-IDENTIFICAR-PERMISSAO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-EXISTE-AGRUPAMENTO.

           EVALUATE WRK-PROX-AGRUPAMENTO

             WHEN 004
                    IF  WRK-5527-CCGC-CPF               NOT EQUAL ZEROS
ST25X6               AND  WRK-5527-CCGC-CPF          NOT EQUAL SPACES
ST25X6               AND  WRK-5527-CCGC-CPF          NOT EQUAL LOW-VALUES
                      PERFORM 3030-ACESSAR-GFCTB0I0

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 3075-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 018         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 009
                  IF  WRK-1-VEZ-SEGMENTO  EQUAL  'N'
                      PERFORM 3035-OBTER-SEG-GESTAO
                      MOVE  'S'  TO  WRK-1-VEZ-SEGMENTO
                  END-IF

                  IF  GFCTJM-CSGMTO-GSTAO-TARIF       NOT EQUAL ZEROS
                      PERFORM 3040-ACESSAR-GFCTB0D2

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 3075-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 'S'         TO WRK-FIM-AGRUPAMENTO
                  END-IF

             WHEN 014
                  PERFORM 3045-ACESSAR-GFCTB0C7

                  IF  WRK-EXISTE-AGRUPAMENTO          EQUAL 'S'
                      PERFORM 3075-OBTER-DADOS-GFCTB0C6
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 015         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 015
                  IF  WRK-1-VEZ-CNPJ-CPF  EQUAL  'N'
                      PERFORM 3025-OBTER-CNPJ-CPF-POSTO
                      MOVE  'S'  TO  WRK-1-VEZ-CNPJ-CPF
                  END-IF

                  IF  WRK-5527-CPOSTO-SERVC           NOT EQUAL ZEROS
                      PERFORM 3050-ACESSAR-GFCTB0D0

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 3075-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 017         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 017
                  IF  WRK-5527-CPOSTO-SERVC           NOT EQUAL ZEROS
                      PERFORM 3055-ACESSAR-GFCTB0D1

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 3075-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 020         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 018
                  IF  WRK-1-VEZ-UF-MUNIC  EQUAL  'N'
                      PERFORM 3060-OBTER-UF-MUNICIPIO
                      MOVE  'S'  TO  WRK-1-VEZ-UF-MUNIC
                  END-IF

                  IF  GFCTRK-CMUN-RURAL               NOT EQUAL ZEROS
                      PERFORM 3065-ACESSAR-GFCTB0C9

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 3075-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 019         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 019
                  IF  GFCTRK-CSGL-UF                  NOT EQUAL SPACES
                      PERFORM 3070-ACESSAR-GFCTB0D3

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 3075-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 009         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 020
                  IF  WRK-5527-CPOSTO-SERVC           NOT EQUAL ZEROS
                      PERFORM 3055-ACESSAR-GFCTB0D1

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 3075-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 004         TO WRK-PROX-AGRUPAMENTO
                  END-IF

           END-EVALUATE.

      *----------------------------------------------------------------*
       3020-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSAR MODULO GFCT5527 PARA OBTER CPF/CNPJ/POSTO.             *
      ******************************************************************
      *----------------------------------------------------------------*
       3025-OBTER-CNPJ-CPF-POSTO       SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                     TO WRK-5527-AREA-SAIDA.

           INITIALIZE WRK-5527-AREA-SAIDA.

           MOVE 'GFCT6101'                 TO WRK-5527-CHAMADOR.
           MOVE GFCTVH-AGENCIA             TO WRK-5527-AGENCIA-CLIENTE.
           MOVE GFCTVH-CONTA               TO WRK-5527-CONTA-CLIENTE.
           MOVE GFCTVH-AMBIENTE            TO WRK-5527-AMBIENTE.

           MOVE 'GFCT5527'                 TO WRK-MODULO.

           CALL WRK-MODULO USING
                                                 WRK-5527-AREA-ENTRADA
                                                 WRK-5527-AREA-SAIDA
                                                 WRK-IO-PCB
                                                 WRK-ALT-PCB
                                                 GFCT0M-AREA-ERROS.


           IF  RETURN-CODE             NOT EQUAL ZEROS
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1319'             TO GFCTG2-COD-MSG
                   MOVE 1319               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1180'             TO GFCTG2-COD-MSG
                   MOVE 1180               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 16                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SPACES             TO GFCTVH-SQLCA
               MOVE 04                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  WRK-5527-COD-RETORNO        NOT EQUAL ZEROS AND 02
                                                           AND 05
               IF  WRK-5527-COD-RETORNO    EQUAL 99
                   IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1325'             TO GFCTG2-COD-MSG
                   MOVE 1325               TO GFCTVH-COD-MENSAGEM
                   ELSE
                   MOVE '1185'             TO GFCTG2-COD-MSG
                   MOVE 1185               TO GFCTVH-COD-MENSAGEM
                   END-IF
                   PERFORM 1500-OBTER-DESC-MSG
                   MOVE 38                  TO GFCTVH-COD-RETORNO
                   MOVE GFCTG3-DESC-MSG        TO GFCTVH-MENSAGEM
                   MOVE SPACES                 TO GFCTVH-SQLCA
                   MOVE 12                     TO RETURN-CODE
                   PERFORM 4000-FINALIZAR
               END-IF
           END-IF.

           IF  WRK-5527-COD-RETORNO        EQUAL 02 OR 05
ST25X6*         MOVE ZEROS              TO WRK-5527-CCGC-CPF
ST25X6*                                    WRK-5527-CFLIAL-CGC
ST25X6*                                    WRK-5527-CCTRL-CPF-CGC
ST25X6          MOVE SPACES             TO WRK-5527-CCGC-CPF
ST25X6                                    WRK-5527-CFLIAL-CGC
ST25X6          MOVE ZEROS              TO WRK-5527-CCTRL-CPF-CGC
           END-IF.

      *----------------------------------------------------------------*
       3025-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO CLIENTE.*
      ******************************************************************
      *----------------------------------------------------------------*
       3030-ACESSAR-GFCTB0I0           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0I0.
           MOVE WRK-PROX-AGRUPAMENTO
                                       TO CAGPTO-CTA        OF GFCTB0I0.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0I0.
ST25X6     move wRK-5527-CCGC-CPF      TO CCGC-CPF-ST       OF GFCTB0I0.
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
ST25X6*            CCGC_CPF           = :GFCTB0I0.CCGC-CPF       
                   CCGC_CPF_ST        = :GFCTB0I0.CCGC-CPF-ST   AND
ST25X6*            CFLIAL_CGC         = :GFCTB0I0.CFLIAL-CGC     
                   CFLIAL_CGC_ST      = :GFCTB0I0.CFLIAL-CGC-ST   AND
ST25X6*            CCTRL_CPF_CGC      = :GFCTB0I0.CCTRL-CPF-CGC  
                   CCTRL_CPF_CGC_ST   = :GFCTB0I0.CCTRL-CPF-CGC-ST   AND
                   DINIC_PRMSS_PCOTE  IN
                (SELECT MAX(DINIC_PRMSS_PCOTE)
                 FROM   DB2PRD.TPRMSS_GRP_CLI
                 WHERE
                 CSERVC_TARIF       = :GFCTB0I0.CSERVC-TARIF      AND
                  CAGPTO_CTA         = :GFCTB0I0.CAGPTO-CTA        AND
ST25X6*            CCGC_CPF           = :GFCTB0I0.CCGC-CPF       
                  CCGC_CPF_ST        = :GFCTB0I0.CCGC-CPF-ST   AND
ST25X6*            CFLIAL_CGC         = :GFCTB0I0.CFLIAL-CGC     
                  CFLIAL_CGC_ST      = :GFCTB0I0.CFLIAL-CGC-ST   AND
ST25X6*            CCTRL_CPF_CGC      = :GFCTB0I0.CCTRL-CPF-CGC  
                  CCTRL_CPF_CGC_ST   = :GFCTB0I0.CCTRL-CPF-CGC-ST   AND
                   DINIC_PRMSS_PCOTE <= :GFCTB0I0.DINIC-PRMSS-PCOTE)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1324'             TO GFCTG2-COD-MSG
                   MOVE 1324               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1184'             TO GFCTG2-COD-MSG
                   MOVE 1184               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 21                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SQLCA(1:136)             TO GFCTVH-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2100-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                         WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA    OF GFCTB0I0 TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0I0 TO WRK-DT-INICIO
                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3030-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSAR MODULO GFCT5534 PARA OBTER SEGMENTO GESTAO.            *
      ******************************************************************
      *----------------------------------------------------------------*
       3035-OBTER-SEG-GESTAO           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTJM-AREA.

           INITIALIZE GFCTJM-AREA.

           MOVE GFCTVH-AMBIENTE TO GFCTJM-TPO-AMBIENTE.
           MOVE GFCTVH-AGENCIA  TO GFCTJM-CJUNC-DEPDC.
           MOVE GFCTVH-CONTA    TO GFCTJM-CCTA-CLI.

           MOVE 'GFCT5534'                 TO WRK-MODULO.

           CALL WRK-MODULO USING
                                                 GFCTJM-AREA
                                                 GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1326'             TO GFCTG2-COD-MSG
                   MOVE 1326               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1187'             TO GFCTG2-COD-MSG
                   MOVE 1187               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 23                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SPACES             TO GFCTVH-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  GFCTJM-COD-RETORNO      EQUAL 99
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1327'             TO GFCTG2-COD-MSG
                   MOVE 1327               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1189'             TO GFCTG2-COD-MSG
                   MOVE 1189               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 24                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SPACES             TO GFCTVH-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  GFCTJM-COD-RETORNO         NOT EQUAL ZEROS
               MOVE ZEROS                 TO GFCTJM-CSGMTO-GSTAO-TARIF
           END-IF.

      *----------------------------------------------------------------*
       3035-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO SEGMENTO*
      ******************************************************************
      *----------------------------------------------------------------*
       3040-ACESSAR-GFCTB0D2           SECTION.
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
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1328'             TO GFCTG2-COD-MSG
                   MOVE 1328               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1190'             TO GFCTG2-COD-MSG
                   MOVE 1190               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 25                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SQLCA(1:136)             TO GFCTVH-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2100-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                         WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA    OF GFCTB0D2 TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0D2 TO WRK-DT-INICIO
                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3040-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO AGENCIA.*
      ******************************************************************
      *----------------------------------------------------------------*
       3045-ACESSAR-GFCTB0C7           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0C7.
           MOVE WRK-PROX-AGRUPAMENTO
                                       TO CAGPTO-CTA        OF GFCTB0C7.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C7.
           MOVE GFCTVH-AGENCIA  TO CDEPDC            OF GFCTB0C7.

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
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1329'             TO GFCTG2-COD-MSG
                   MOVE 1329               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1191'             TO GFCTG2-COD-MSG
                   MOVE 1191               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 26                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SQLCA(1:136)             TO GFCTVH-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2100-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                         WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA    OF GFCTB0C7 TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0C7 TO WRK-DT-INICIO
                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3045-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO PAB.    *
      ******************************************************************
      *----------------------------------------------------------------*
       3050-ACESSAR-GFCTB0D0           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D0.
           MOVE WRK-PROX-AGRUPAMENTO
                                       TO CAGPTO-CTA        OF GFCTB0D0.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0D0.
           MOVE GFCTVH-AGENCIA  TO CDEPDC            OF GFCTB0D0.

           MOVE WRK-5527-CPOSTO-SERVC TO CPOSTO-SERVC      OF GFCTB0D0.

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
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1330'             TO GFCTG2-COD-MSG
                   MOVE 1330               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1192'             TO GFCTG2-COD-MSG
                   MOVE 1192               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 27                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SQLCA(1:136)             TO GFCTVH-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2100-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                         WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA    OF GFCTB0D0 TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0D0 TO WRK-DT-INICIO
                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3050-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO PSTAL.  *
      ******************************************************************
      *----------------------------------------------------------------*
       3055-ACESSAR-GFCTB0D1           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D1.
           MOVE WRK-PROX-AGRUPAMENTO
                                       TO CAGPTO-CTA        OF GFCTB0D1.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0D1.
           MOVE GFCTVH-AGENCIA  TO CDEPDC            OF GFCTB0D1.

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
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1331'             TO GFCTG2-COD-MSG
                   MOVE 1331               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1193'             TO GFCTG2-COD-MSG
                   MOVE 1193               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 28                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SQLCA(1:136)             TO GFCTVH-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2100-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                         WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA    OF GFCTB0D1 TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0D1 TO WRK-DT-INICIO
                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3055-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSAR MODULO GFCT5564 PARA OBTER UF E MUNICIPIO.             *
      ******************************************************************
      *----------------------------------------------------------------*
       3060-OBTER-UF-MUNICIPIO         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTRK-AREA.

           INITIALIZE GFCTRK-AREA.

           MOVE GFCTVH-AMBIENTE TO GFCTRK-TPO-AMBIENTE.
           MOVE GFCTVH-AGENCIA  TO GFCTRK-CJUNC-DEPDC.

           MOVE 'GFCT5564'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTRK-AREA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1332'             TO GFCTG2-COD-MSG
                   MOVE 1332               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1194'             TO GFCTG2-COD-MSG
                   MOVE 1194               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 29                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE 12                 TO RETURN-CODE
               MOVE SPACES             TO GFCTVH-SQLCA
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  GFCTRK-COD-RETORNO      EQUAL 01 OR 02 OR 99
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1333'             TO GFCTG2-COD-MSG
                   MOVE 1333               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1195'             TO GFCTG2-COD-MSG
                   MOVE 1195               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 30                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SPACES             TO GFCTVH-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  GFCTRK-COD-RETORNO      NOT EQUAL ZEROS
               MOVE ZEROS              TO GFCTRK-CMUN-RURAL
               MOVE SPACES             TO GFCTRK-CSGL-UF
           END-IF.

      *----------------------------------------------------------------*
       3060-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GP MUNICIPIO. *
      ******************************************************************
      *----------------------------------------------------------------*
       3065-ACESSAR-GFCTB0C9           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0C9.
           MOVE WRK-PROX-AGRUPAMENTO
                                       TO CAGPTO-CTA        OF GFCTB0C9.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C9.
           MOVE GFCTRK-CMUN-RURAL      TO WRK-CAMPO-9.
           MOVE WRK-CAMPO-9-7          TO WRK-CAMPO-S7-7.
           MOVE WRK-CAMPO-S7           TO CMUN-IBGE         OF GFCTB0C9.


______*____MOVE_GFCTRK-CMUN-RURAL______TO_CMUN-IBGE_________OF_GFCTB0C9.

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
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1334'             TO GFCTG2-COD-MSG
                   MOVE 1334               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1196'             TO GFCTG2-COD-MSG
                   MOVE 1196               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 31                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SQLCA(1:136)             TO GFCTVH-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2100-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                         WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA    OF GFCTB0C9 TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0C9 TO WRK-DT-INICIO
                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3065-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO UF.     *
      ******************************************************************
      *----------------------------------------------------------------*
       3070-ACESSAR-GFCTB0D3           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D3.
           MOVE WRK-PROX-AGRUPAMENTO
                                       TO CAGPTO-CTA        OF GFCTB0D3.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
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
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1335'             TO GFCTG2-COD-MSG
                   MOVE 1335               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1197'             TO GFCTG2-COD-MSG
                   MOVE 1197               TO GFCTVH-COD-MENSAGEM
               END-IF

               PERFORM 1500-OBTER-DESC-MSG
               MOVE 32                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SQLCA(1:136)             TO GFCTVH-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2100-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                         WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA    OF GFCTB0D3 TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0D3 TO WRK-DT-INICIO
                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3070-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER DADOS DA PERMISSAO DE ADESAO AO PACOTE.                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3075-OBTER-DADOS-GFCTB0C6       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0C6.
           MOVE WRK-SEQ-AGPTO          TO CSEQ-AGPTO-CTA    OF GFCTB0C6.
           MOVE WRK-DT-INICIO          TO DINIC-PRMSS-PCOTE OF GFCTB0C6.
           MOVE 'I'                    TO CINDCD-ADSAO-INDVD
                                                            OF GFCTB0C6.


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
               AND CINDCD_ADSAO_INDVD  = :GFCTB0C6.CINDCD-ADSAO-INDVD
               AND CIDTFD_PRMSS_INDVD  IN ( 0, 2 )
               AND DFIM_PRMSS_PCOTE    > :GFCTB0C6.DFIM-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -811) OR
              (SQLWARN0                EQUAL 'W'      )
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1336'             TO GFCTG2-COD-MSG
                   MOVE 1336               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1198'             TO GFCTG2-COD-MSG
                   MOVE 1198               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 33                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SQLCA(1:136)             TO GFCTVH-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS OR -811
               MOVE 'S'                TO WRK-AGRUPAMENTO-VALIDO
           END-IF.

      *----------------------------------------------------------------*
       3075-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-VER-TARIFA-COMP-CAD        SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                      TO WRK-FIM-CURSOR
                                            WRK-EXISTE-COMPONENTE.

           PERFORM 3110-ABRIR-CURSOR.

           PERFORM 3120-LER-CURSOR.

           PERFORM 3130-PROCESSAR-CURSOR  UNTIL
                   WRK-FIM-CURSOR         EQUAL 'S' OR
                   WRK-EXISTE-COMPONENTE  EQUAL 'S'.

           PERFORM 3140-FECHAR-CURSOR.

           IF  WRK-EXISTE-COMPONENTE   EQUAL 'S'
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1393'             TO GFCTG2-COD-MSG
                   MOVE 1393               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1211'             TO GFCTG2-COD-MSG
                   MOVE 1211               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 50                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SPACES             TO GFCTVH-SQLCA
               MOVE 04                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ABRIR CURSOR DA TABELA GFCTB029.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       3110-ABRIR-CURSOR               SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVH-PACOTE   TO CTARIF-PCOTE     OF GFCTB029.

           EXEC SQL
               OPEN CSR01-GFCTB029
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1396'             TO GFCTG2-COD-MSG
                   MOVE 1396               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1207'             TO GFCTG2-COD-MSG
                   MOVE 1207               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 44                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SQLCA(1:136)             TO GFCTVH-SQLCA
               MOVE 04                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       3110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * LER   CURSOR DA TABELA GFCTB029.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       3120-LER-CURSOR                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR01-GFCTB029 INTO
                   :GFCTB029.CTARIF-COMPO-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1397'             TO GFCTG2-COD-MSG
                   MOVE 1397               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1208'             TO GFCTG2-COD-MSG
                   MOVE 1208               TO GFCTVH-COD-MENSAGEM
               END-IF

               PERFORM 1500-OBTER-DESC-MSG
               MOVE 45                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SQLCA(1:136)             TO GFCTVH-SQLCA
               MOVE 04                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR
           END-IF.

      *----------------------------------------------------------------*
       3120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR CURSOR DA TABELA GFCTB029.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3130-PROCESSAR-CURSOR           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVH-AGENCIA  TO CJUNC-DEPDC      OF GFCTB009.
           MOVE GFCTVH-CONTA    TO CCTA-CLI         OF GFCTB009.
           MOVE ZEROS           TO CINDCD-EXCL-REG  OF GFCTB009.

           MOVE WRK-DIA-INI-ADSAO-INDVD
                                TO DFIM-ADSAO-INDVD OF GFCTB009(1:2).
           MOVE WRK-MES-INI-ADSAO-INDVD
                                TO DFIM-ADSAO-INDVD OF GFCTB009(4:2).
           MOVE WRK-ANO-INI-ADSAO-INDVD
                                TO DFIM-ADSAO-INDVD OF GFCTB009(7:4).
           MOVE '.'             TO DFIM-ADSAO-INDVD OF GFCTB009(3:1)
                                   DFIM-ADSAO-INDVD OF GFCTB009(6:1).

           EXEC SQL
             SELECT DISTINCT (B.CTARIF_PCOTE)
             INTO
                   :GFCTB029.CTARIF-PCOTE
             FROM   DB2PRD.ADSAO_INDVD_PCOTE A,
                    DB2PRD.COMPS_TARIF_PCOTE B
             WHERE
               A.CJUNC_DEPDC        = :GFCTB009.CJUNC-DEPDC       AND
               A.CCTA_CLI           = :GFCTB009.CCTA-CLI          AND
               A.CINDCD_EXCL_REG    = :GFCTB009.CINDCD-EXCL-REG   AND
               A.DFIM_ADSAO_INDVD  >= :GFCTB009.DFIM-ADSAO-INDVD  AND
               A.CSERVC_TARIF       = B.CTARIF_PCOTE              AND
               B.CTARIF_COMPO_PCOTE = :GFCTB029.CTARIF-COMPO-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                       AND -811) OR
              (SQLWARN0                EQUAL 'W'      )
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1394'             TO GFCTG2-COD-MSG
                   MOVE 1394               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1209'             TO GFCTG2-COD-MSG
                   MOVE 1209               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 46                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SQLCA(1:136)             TO GFCTVH-SQLCA
               MOVE 04                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS OR -811
               MOVE 'S'                TO WRK-EXISTE-COMPONENTE
           END-IF.

           PERFORM 3120-LER-CURSOR.

      *----------------------------------------------------------------*
       3130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * FECHAR CURSOR DA TABELA GFCTB029.                              *
      ******************************************************************
      *----------------------------------------------------------------*
       3140-FECHAR-CURSOR              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-GFCTB029
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1382'             TO GFCTG2-COD-MSG
                   MOVE 1382               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1210'             TO GFCTG2-COD-MSG
                   MOVE 1210               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 47                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SQLCA(1:136)             TO GFCTVH-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       3140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3150-VER-TARIFA-PACOTE-CAD      SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVH-AGENCIA         TO CJUNC-DEPDC       OF GFCTB009.
           MOVE GFCTVH-CONTA           TO CCTA-CLI          OF GFCTB009.
PROC*******MOVE_GFCTVH-PACOTE__________TO_CSERVC-TARIF______OF_GFCTB009.
           MOVE WRK-DIA-INI-ADSAO-INDVD
                                TO DFIM-ADSAO-INDVD OF GFCTB009(1:2).
           MOVE WRK-MES-INI-ADSAO-INDVD
                                TO DFIM-ADSAO-INDVD OF GFCTB009(4:2).
           MOVE WRK-ANO-INI-ADSAO-INDVD
                                TO DFIM-ADSAO-INDVD OF GFCTB009(7:4).
           MOVE '.'             TO DFIM-ADSAO-INDVD OF GFCTB009(3:1)
                                   DFIM-ADSAO-INDVD OF GFCTB009(6:1).
           MOVE 0                      TO CINDCD-EXCL-REG   OF GFCTB009.

           EXEC SQL
             SELECT
                   DINIC_ADSAO_INDVD,
                   DFIM_ADSAO_INDVD
             INTO
                   :GFCTB009.DINIC-ADSAO-INDVD,
                   :GFCTB009.DFIM-ADSAO-INDVD
             FROM   DB2PRD.ADSAO_INDVD_PCOTE
             WHERE
                   CJUNC_DEPDC         = :GFCTB009.CJUNC-DEPDC       AND
                   CCTA_CLI            = :GFCTB009.CCTA-CLI          AND
PROC***************CSERVC_TARIF        = :GFCTB009.CSERVC-TARIF      AND
                   CINDCD_EXCL_REG     = :GFCTB009.CINDCD-EXCL-REG   AND
                   DFIM_ADSAO_INDVD   >  :GFCTB009.DFIM-ADSAO-INDVD
           END-EXEC.

           IF (SQLCODE          NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0                EQUAL 'W'      )
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1343'             TO GFCTG2-COD-MSG
                   MOVE 1343               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1205'             TO GFCTG2-COD-MSG
                   MOVE 1205               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 40                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SQLCA(1:136)             TO GFCTVH-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF (SQLCODE              EQUAL ZEROS OR -811)
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1342'             TO GFCTG2-COD-MSG
                   MOVE 1342               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1204'             TO GFCTG2-COD-MSG
                   MOVE 1204               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 39                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SQLCA(1:136)             TO GFCTVH-SQLCA
               MOVE 04                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       3150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * MOVIMENTAR AS INFORMACOES PARA AREA DE SAIDA.                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-INSERIR-GFCTB009         SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVH-AGENCIA         TO CJUNC-DEPDC      OF GFCTB009.
           MOVE GFCTVH-CONTA           TO CCTA-CLI         OF GFCTB009.
           MOVE GFCTVH-PACOTE          TO CSERVC-TARIF     OF GFCTB009.
           MOVE 6101                   TO CFUNC-MANUT-INCL OF GFCTB009.
           MOVE ZEROS                  TO CFUNC-MANUT      OF GFCTB009.

           MOVE WRK-DIA-INI-ADSAO-INDVD
                                TO DINIC-ADSAO-INDVD OF GFCTB009(1:2).
           MOVE WRK-MES-INI-ADSAO-INDVD
                                TO DINIC-ADSAO-INDVD OF GFCTB009(4:2).
           MOVE WRK-ANO-INI-ADSAO-INDVD
                                TO DINIC-ADSAO-INDVD OF GFCTB009(7:4).
           MOVE '.'             TO DINIC-ADSAO-INDVD OF GFCTB009(3:1)
                                   DINIC-ADSAO-INDVD OF GFCTB009(6:1).
           MOVE WRK-DIA-FIM-ADSAO-INDVD
                                TO DFIM-ADSAO-INDVD  OF GFCTB009(1:2).
           MOVE WRK-MES-FIM-ADSAO-INDVD
                                TO DFIM-ADSAO-INDVD  OF GFCTB009(4:2).
           MOVE WRK-ANO-FIM-ADSAO-INDVD
                                TO DFIM-ADSAO-INDVD  OF GFCTB009(7:4).
           MOVE '.'             TO DFIM-ADSAO-INDVD  OF GFCTB009(3:1)
                                   DFIM-ADSAO-INDVD  OF GFCTB009(6:1).

           MOVE '0001-01-01-01.01.01.000001'
                                       TO HMANUT-REG       OF GFCTB009.
           MOVE ZEROS                  TO CINDCD-EXCL-REG  OF GFCTB009.

           EXEC SQL
             INSERT INTO DB2PRD.ADSAO_INDVD_PCOTE
                  (CJUNC_DEPDC,
                   CCTA_CLI,
                   CSERVC_TARIF,
                   HINCL_REG,
                   CFUNC_MANUT_INCL,
                   CFUNC_MANUT,
                   DINIC_ADSAO_INDVD,
                   DFIM_ADSAO_INDVD,
                   HMANUT_REG,
                   CINDCD_EXCL_REG)
               VALUES
                  (:GFCTB009.CJUNC-DEPDC,
                   :GFCTB009.CCTA-CLI,
                   :GFCTB009.CSERVC-TARIF,
                    CURRENT TIMESTAMP,
                   :GFCTB009.CFUNC-MANUT-INCL,
                   :GFCTB009.CFUNC-MANUT,
                   :GFCTB009.DINIC-ADSAO-INDVD,
                   :GFCTB009.DFIM-ADSAO-INDVD,
                   :GFCTB009.HMANUT-REG,
                   :GFCTB009.CINDCD-EXCL-REG)
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +803) OR
              (SQLWARN0                 EQUAL 'W'      )
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1343'             TO GFCTG2-COD-MSG
                   MOVE 1343               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1205'             TO GFCTG2-COD-MSG
                   MOVE 1205               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 40                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SQLCA(1:136)             TO GFCTVH-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF SQLCODE EQUAL ZEROS
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1399'             TO GFCTG2-COD-MSG
                   MOVE 1399               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1201'             TO GFCTG2-COD-MSG
                   MOVE 1201               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 35                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SPACES             TO GFCTVH-SQLCA
               MOVE ZEROS              TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF SQLCODE EQUAL +803
               IF GFCTVH-AMBIENTE EQUAL 'O'
                   MOVE '1342'             TO GFCTG2-COD-MSG
                   MOVE 1342               TO GFCTVH-COD-MENSAGEM
               ELSE
                   MOVE '1204'             TO GFCTG2-COD-MSG
                   MOVE 1204               TO GFCTVH-COD-MENSAGEM
               END-IF
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 39                 TO GFCTVH-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTVH-MENSAGEM
               MOVE SQLCA(1:136)             TO GFCTVH-SQLCA
               MOVE 04                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
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








