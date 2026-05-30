      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT6042.
       AUTHOR.     LUIZ MOREIRA XAVIER.
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
      *    PROGRAMA....:   GFCT6042                                    *
      *    PROGRAMADOR.:   LUIZ MOREIRA XAVIER - PROCWORK/GP50         *
      *    ANALISTA....:   LUIZ MOREIRA XAVIER - PROCWORK/GP50         *
      *    DATA........:   21/09/2015                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   MODULO LISTA DE CESTAS PERMITIDAS           *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                     INCLUDE/BOOK        *
      *                   DB2PRD.PARM_DATA_PROCM     GFCTB0A1          *
      *                   DB2PRD.PARM_SERVC_TARIF    GFCTB0A2          *
      *                   DB2PRD.PRMSS_ADSAO_PCOTE   GFCTB0C6          *
      *                   DB2PRD.PRMSS_GRP_DEPDC     GFCTB0C7          *
      *                   DB2PRD.PRMSS_GRP_MUN       GFCTB0C9          *
      *                   DB2PRD.PRMSS_GRP_PAB       GFCTB0D0          *
      *                   DB2PRD.PRMSS_GRP_PSTAL     GFCTB0D1          *
      *                   DB2PRD.PRMSS_GRP_SGMTO     GFCTB0D2          *
      *                   DB2PRD.PRMSS_GRP_UF        GFCTB0D3          *
      *                   DB2PRD.SERVC_TARIF_PRINC   GFCTB0D8          *
      *                   DB2PRD.TPRMSS_GRP_CLI      GFCTB0I0          *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#CMTFWY4I - ENTRADA - VERIFICAR PACOTES/TARIFAS VIGENTES.  *
      *    I#CMTFWY4I - SAIDA - VERIFICAR PACOTES/TARIFAS VIGENTES.    *
      *    I#GFCTG2 - ENTRADA - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTG3 - SAIDA   - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTJM - ENTRADA E SAIDA - OBTER COD SEG GESTAO TARIFA.   *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *    I#GFCTRK - ENTRADA E SAIDA - OBTER COD UF E MUNICIPIO DA    *
      *               AGENCIA.                                         *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    GFCT5534 - OBTER CODIGO SEGMENTO GESTAO TARIFA.             *
      *    GFCT5564 - OBTER COD DA UF E MUNICIPIO DA AGENCIA.          *
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
       77  WRK-TIPO-CONTA-CLIENTE      PIC S9(001) COMP-3  VALUE ZEROS.
       77  WRK-RESTO                   PIC S9(003) COMP-3  VALUE ZEROS.
       77  WRK-INTEIRO                 PIC S9(005) COMP-3  VALUE ZEROS.
       77  WRK-NUM-CURSOR              PIC  9(002)         VALUE ZEROS.
       77  WRK-FIM-CURSOR              PIC  X(001)         VALUE SPACES.
       77  WRK-TARIFA-VALIDA           PIC  X(001)         VALUE SPACES.
       77  WRK-FIM-AGRUPAMENTO         PIC  X(001)         VALUE SPACES.
       77  WRK-EXISTE-AGRUPAMENTO      PIC  X(001)         VALUE SPACES.
       77  WRK-AGRUPAMENTO-VALIDO      PIC  X(001)         VALUE SPACES.
       77  WRK-PROX-AGRUPAMENTO        PIC  9(003)         VALUE ZEROS.
       77  WRK-SEQ-AGPTO               PIC S9(009) COMP-3  VALUE ZEROS.
       77  WRK-DT-INICIO               PIC  X(010)         VALUE SPACES.
       77  WRK-1-VEZ-UF-MUNIC          PIC  X(001)         VALUE 'N'.
       77  WRK-CESTA-CELULAR           PIC  9(001)         VALUE ZEROS.
RJOVA  77  WRK-BANCO-POSTAL            PIC  X(001)         VALUE SPACES.
       77  WRK-CSGMTO-GSTAO-TARIF-I    PIC S9(003) COMP-3  VALUE ZEROS.
       77  WRK-CSGMTO-GSTAO-TARIF-F    PIC S9(003) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA AUXILIARES *'.
      *----------------------------------------------------------------*

       01  IND-1                       PIC  9(003) COMP-3  VALUE ZEROS.
       01  IND-2                       PIC  9(003) COMP-3  VALUE ZEROS.

       01  WRK-CMTFWY4I-E-CD-COML-AGENCIA
                                       PIC S9(006) COMP-3  VALUE ZEROS.

       01  WRK-CDEPDC-5   REDEFINES    WRK-CMTFWY4I-E-CD-COML-AGENCIA.
           03 FILLER                   PIC  X(001).
           03 WRK-CDEPDC               PIC S9(005) COMP-3.
           03 WRK-CDEPDC-S             REDEFINES   WRK-CDEPDC
                                       PIC  9(005) COMP-3.

       01  WRK-GFCTRK-CMUN-RURAL       PIC S9(009) COMP-3  VALUE ZEROS.

       01  WRK-GFCTRK-CMUN-RURAL-7     REDEFINES  WRK-GFCTRK-CMUN-RURAL.
           03 FILLER                   PIC  X(001).
           03 WRK-CMUN-RURAL           PIC S9(007) COMP-3.

       01  WRK-TAM-04-N                PIC  9(004)         VALUE ZEROS.
       01  FILLER REDEFINES WRK-TAM-04-N.
           05  WRK-TAM-04-A            PIC  X(004).

       01  WRK-CMTFWY4I-SAIDA                              VALUE SPACES.
           03 FILLER                   PIC  X(003).
           03 WRK-CMTFWY4I-S-REG-LISTA PIC  X(103)         OCCURS 150.

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
                                       PIC  9(004).
               05  WRK-MES-INI-ADSAO-INDVD
                                       PIC  9(002).
           03  WRK-DIA-INI-ADSAO-INDVD PIC  9(002).
       01  WRK-DT-INI-ADSAO-INDVD-A    REDEFINES WRK-DT-INI-ADSAO-INDVD.
           03  WRK-ANO-INI-ADSAO-INDVD-A
                                       PIC  X(004).
           03  WRK-MES-INI-ADSAO-INDVD-A
                                       PIC  X(002).
           03  WRK-DIA-INI-ADSAO-INDVD-A
                                       PIC  X(002).

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
       01  WRK-DT-FIM-ADSAO-INDVD-A    REDEFINES WRK-DT-FIM-ADSAO-INDVD.
           03  WRK-ANO-FIM-ADSAO-INDVD-A
                                       PIC  X(004).
           03  WRK-MES-FIM-ADSAO-INDVD-A
                                       PIC  X(002).
           03  WRK-DIA-FIM-ADSAO-INDVD-A
                                       PIC  X(002).

       01  WRK-DT-AUX-C.
           03  WRK-ANO-AUX-C           PIC  9(004) COMP-3  VALUE ZEROS.
           03  WRK-MES-AUX-C           PIC  9(002) COMP-3  VALUE ZEROS.

       01  WRK-CINDCD-ADSAO-INDVD      PIC  X(001)         VALUE 'I'.
       01  WRK-CINDCD-ADSAO-AMBAS      PIC  X(001)         VALUE 'A'.
RJOVA  01  WRK-1-VEZ-SEGMENTO          PIC  X(001)         VALUE 'N'.

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
           '* AREA PARA INCLUDES *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

       COPY 'I#GFCTRK'.

       COPY 'I#GFCT0M'.

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
             INCLUDE GFCTB0D8
           END-EXEC.

RJOVA *--- GFCTB0H3 - DB2PRD.TASSOC_SGMTO_CLI                       ---*

           EXEC SQL
                    INCLUDE GFCTB0H3
           END-EXEC.

RJOVA *--- GFCTB0H5 - DB2PRD.TTPO_SGMTO_GSTAO                       ---*

           EXEC SQL
                    INCLUDE GFCTB0H5
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DECLARACAO DE CURSORES *'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR01-GFCTB0C6-JN CURSOR FOR
             SELECT DISTINCT(A.CSERVC_TARIF)
             FROM  DB2PRD.PRMSS_ADSAO_PCOTE A,
                   DB2PRD.SERVC_TARIF_PRINC B
             WHERE
                   A.CSERVC_TARIF       = B.CSERVC_TARIF             AND
                  (A.CINDCD_ADSAO_INDVD = :WRK-CINDCD-ADSAO-INDVD    OR
                   A.CINDCD_ADSAO_INDVD = :WRK-CINDCD-ADSAO-AMBAS)   AND
                   A.DFIM_PRMSS_PCOTE   > :GFCTB0C6.DFIM-PRMSS-PCOTE AND
                   A.CSERVC_TARIF       > :GFCTB0C6.CSERVC-TARIF     AND
                   B.CTPO_SERVC_TARIF   = 2
             ORDER BY CSERVC_TARIF ASC
           END-EXEC.

           EXEC SQL
             DECLARE CSR02-GFCTB0C6-JN CURSOR FOR
             SELECT DISTINCT(A.CSERVC_TARIF)
             FROM  DB2PRD.PRMSS_ADSAO_PCOTE A,
                   DB2PRD.SERVC_TARIF_PRINC B
             WHERE
                   A.CSERVC_TARIF       = B.CSERVC_TARIF             AND
                  (A.CINDCD_ADSAO_INDVD = :WRK-CINDCD-ADSAO-INDVD    OR
                   A.CINDCD_ADSAO_INDVD = :WRK-CINDCD-ADSAO-AMBAS)   AND
                   A.DFIM_PRMSS_PCOTE   > :GFCTB0C6.DFIM-PRMSS-PCOTE AND
                   A.CSERVC_TARIF       < :GFCTB0C6.CSERVC-TARIF     AND
                   B.CTPO_SERVC_TARIF   = 2
             ORDER BY CSERVC_TARIF DESC
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-CMTFWY4I.
       COPY 'CMTFWY4I'.

      *================================================================*
       PROCEDURE                       DIVISION USING LNK-CMTFWY4I.
      *================================================================*

      ******************************************************************
      * ROTINA PRINCIPAL.                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           IF CMTFWY4I-E-AMBIENTE EQUAL 'B' OR 'b'
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

           INITIALIZE                  CMTFWY4I-SAIDA
                                       REPLACING ALPHANUMERIC BY SPACES.
           MOVE ZEROS                  TO CMTFWY4I-COD-RETORNO
                                          CMTFWY4I-COD-MENSAGEM.
           MOVE SPACES                 TO CMTFWY4I-MENSAGEM
                                          CMTFWY4I-SQLCA.

           PERFORM 1100-CONSISTIR-DADOS.

           PERFORM 1500-MONTAR-VARIAVEIS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONSISTENCIA DOS DADOS DE ENTRADA.                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (CMTFWY4I-E-CD-COML-AGENCIA NOT NUMERIC)              OR
              (CMTFWY4I-E-CD-COML-AGENCIA EQUAL ZEROS)              OR
              (CMTFWY4I-E-AMBIENTE      NOT EQUAL 'O' AND 'B'
                                              AND 'o' AND 'b')
               MOVE 'B'                 TO CMTFWY4I-E-AMBIENTE
               MOVE 1166                TO CMTFWY4I-COD-MENSAGEM
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 08                  TO CMTFWY4I-COD-RETORNO
               MOVE GFCTG3-DESC-MSG     TO CMTFWY4I-MENSAGEM
               MOVE SPACES              TO CMTFWY4I-SQLCA
               MOVE 12                  TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF CMTFWY4I-E-AMBIENTE EQUAL 'o'
              MOVE 'O' TO CMTFWY4I-E-AMBIENTE
           END-IF.

           IF CMTFWY4I-E-AMBIENTE EQUAL 'b'
              MOVE 'B' TO CMTFWY4I-E-AMBIENTE
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
           MOVE 'GFCT6042'             TO GFCTG2-TRANSACAO
                                          GFCTG2-FUNCAO.
           MOVE '1'                    TO GFCTG2-FUNC-BDSCO.
           MOVE CMTFWY4I-E-AMBIENTE    TO GFCTG2-TIPO-PROC.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE CMTFWY4I-COD-MENSAGEM  TO WRK-TAM-04-N.
           MOVE WRK-TAM-04-A           TO GFCTG2-COD-MSG.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO             USING
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 12                 TO CMTFWY4I-COD-RETORNO
               MOVE 1305               TO CMTFWY4I-COD-MENSAGEM
               MOVE WRK-MSG-1          TO CMTFWY4I-MENSAGEM
               MOVE SPACES             TO CMTFWY4I-SQLCA
               MOVE 04                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 12                 TO CMTFWY4I-COD-RETORNO
               MOVE 1306               TO CMTFWY4I-COD-MENSAGEM
               MOVE WRK-MSG-2          TO CMTFWY4I-MENSAGEM
               MOVE GFCT0M-SQLCA-AREA(1:LENGTH OF CMTFWY4I-SQLCA)
                                       TO CMTFWY4I-SQLCA
               MOVE 04                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE 12                 TO CMTFWY4I-COD-RETORNO
               MOVE 1307               TO CMTFWY4I-COD-MENSAGEM
               MOVE WRK-MSG-3          TO CMTFWY4I-MENSAGEM
               MOVE SPACES             TO CMTFWY4I-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * MONTAR VARIAVEIS PARA UTILIZAO NO PROGRAMA                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1500-MONTAR-VARIAVEIS           SECTION.
      *----------------------------------------------------------------*

           MOVE CMTFWY4I-E-CD-COML-AGENCIA
                                     TO WRK-CMTFWY4I-E-CD-COML-AGENCIA.
           IF   CMTFWY4I-E-CD-TIPO-CONTA EQUAL 01
                MOVE 1               TO WRK-TIPO-CONTA-CLIENTE
           ELSE
                MOVE 2               TO WRK-TIPO-CONTA-CLIENTE
           END-IF.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA DE PROCESSAMENTO PRINCIPAL DO PROGRAMA.                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO IND-1.
           MOVE 'N'                    TO WRK-FIM-CURSOR.

           PERFORM 2100-OBTER-DATA-PROCESSAMENTO.

           PERFORM 2200-ABRIR-CURSOR.

           PERFORM 2300-LER-CURSOR.

           PERFORM 2400-PROCESSAR-CURSOR   UNTIL
                   WRK-FIM-CURSOR          EQUAL 'S'.

           PERFORM 2500-FECHAR-CURSOR.

           PERFORM 2600-ORDENAR-OCORRENCIAS.

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
               IF CMTFWY4I-E-AMBIENTE EQUAL 'O'
                   MOVE 1309               TO CMTFWY4I-COD-MENSAGEM
               ELSE
                   MOVE 1170               TO CMTFWY4I-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 12                 TO CMTFWY4I-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO CMTFWY4I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY4I-SQLCA)
                                       TO CMTFWY4I-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ABRIR CURSOR DA TABELA GFCTB0C6                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-ABRIR-CURSOR               SECTION.
      *----------------------------------------------------------------*

           EVALUATE CMTFWY4I-INDICADOR-PAGINACAO
               WHEN 'I'
               WHEN 'P'
                    MOVE 01            TO WRK-NUM-CURSOR
                    MOVE ZEROS         TO CSERVC-TARIF OF GFCTB0C6
               WHEN 'S'
                    MOVE 01            TO WRK-NUM-CURSOR
                    MOVE CMTFWY4I-I-CD-CESTA-PERM
                                       TO CSERVC-TARIF OF GFCTB0C6
               WHEN 'A'
                    MOVE 02            TO WRK-NUM-CURSOR
                    MOVE CMTFWY4I-F-CD-CESTA-PERM
                                       TO CSERVC-TARIF OF GFCTB0C6
               WHEN 'U'
                    MOVE 02            TO WRK-NUM-CURSOR
                    MOVE 99999         TO CSERVC-TARIF OF GFCTB0C6
               WHEN OTHER
                    MOVE 01            TO WRK-NUM-CURSOR
                    MOVE ZEROS         TO CSERVC-TARIF OF GFCTB0C6
           END-EVALUATE.

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DFIM-PRMSS-PCOTE  OF GFCTB0C6.

           MOVE 'I'                    TO WRK-CINDCD-ADSAO-INDVD.
           MOVE 'A'                    TO WRK-CINDCD-ADSAO-AMBAS.

           IF   WRK-NUM-CURSOR EQUAL 01
                EXEC SQL
                     OPEN CSR01-GFCTB0C6-JN
                END-EXEC
           ELSE
                EXEC SQL
                     OPEN CSR02-GFCTB0C6-JN
                END-EXEC
           END-IF.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               IF CMTFWY4I-E-AMBIENTE EQUAL 'O'
                   MOVE 1310               TO CMTFWY4I-COD-MENSAGEM
               ELSE
                   MOVE 1171               TO CMTFWY4I-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 12                 TO CMTFWY4I-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO CMTFWY4I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY4I-SQLCA)
                                       TO CMTFWY4I-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * LER CURSOR DA TABELA GFCTB0C6                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-LER-CURSOR                 SECTION.
      *----------------------------------------------------------------*

           IF   WRK-NUM-CURSOR EQUAL 01
                EXEC SQL
                     FETCH CSR01-GFCTB0C6-JN INTO
                     :GFCTB0C6.CSERVC-TARIF
                END-EXEC
           ELSE
                EXEC SQL
                     FETCH CSR02-GFCTB0C6-JN INTO
                     :GFCTB0C6.CSERVC-TARIF
                END-EXEC
           END-IF.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               IF CMTFWY4I-E-AMBIENTE EQUAL 'O'
                   MOVE 1311               TO CMTFWY4I-COD-MENSAGEM
               ELSE
                   MOVE 1171               TO CMTFWY4I-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 12                 TO CMTFWY4I-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO CMTFWY4I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY4I-SQLCA)
                                       TO CMTFWY4I-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR
               IF  IND-1               EQUAL ZEROS
                   PERFORM 2500-FECHAR-CURSOR
                   IF CMTFWY4I-E-AMBIENTE EQUAL 'O'
                      MOVE  1312              TO CMTFWY4I-COD-MENSAGEM
                   ELSE
                      MOVE  1173              TO CMTFWY4I-COD-MENSAGEM
                   END-IF
                   PERFORM 1200-OBTER-DESC-MSG
                   MOVE 08                 TO CMTFWY4I-COD-RETORNO
                   MOVE GFCTG3-DESC-MSG    TO CMTFWY4I-MENSAGEM
                   MOVE SQLCA(1:LENGTH OF CMTFWY4I-SQLCA)
                                           TO CMTFWY4I-SQLCA
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
               IF CMTFWY4I-E-AMBIENTE EQUAL 'O'
                   MOVE 1318                TO CMTFWY4I-COD-MENSAGEM
               ELSE
                   MOVE 1198                TO CMTFWY4I-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 12                  TO CMTFWY4I-COD-RETORNO
               MOVE GFCTG3-DESC-MSG     TO CMTFWY4I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY4I-SQLCA)
                                        TO CMTFWY4I-SQLCA
               MOVE 12                  TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  CINDCD-CREDT-CLULR OF GFCTB0A2   EQUAL   'S'
               MOVE 1                  TO WRK-CESTA-CELULAR
           ELSE
               MOVE 0                  TO WRK-CESTA-CELULAR
           END-IF.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR DADOS OBTIDOS NO CURSOR 1 DA TABELA GFCTB0C6.        *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-PROCESSAR-CURSOR           SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-TARIFA-VALIDA.

           PERFORM 2410-OBTER-DADOS-GFCTB0D8.

           IF  CTPO-CTA-DEB-TARIF      OF GFCTB0D8
                                       EQUAL WRK-TIPO-CONTA-CLIENTE
               IF  CPSSOA-SERVC-TARIF  OF GFCTB0D8 EQUAL '3'  OR
                  (CPSSOA-SERVC-TARIF  OF GFCTB0D8 EQUAL '1'  AND
                   CMTFWY4I-E-CTPO-PSSOA         EQUAL 'F')   OR
                  (CPSSOA-SERVC-TARIF  OF GFCTB0D8 EQUAL '2'  AND
                   CMTFWY4I-E-CTPO-PSSOA         EQUAL 'J')
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
               PERFORM 2300-LER-CURSOR
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
               IF CMTFWY4I-E-AMBIENTE EQUAL 'O'
                   MOVE 1317               TO CMTFWY4I-COD-MENSAGEM
               ELSE
                   MOVE 1178               TO CMTFWY4I-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 12                 TO CMTFWY4I-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO CMTFWY4I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY4I-SQLCA)
                                       TO CMTFWY4I-SQLCA
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
                                       TO WRK-DIA-FIM-ADSAO-INDVD-A.
           MOVE DFIM-VGCIA-TARIF       OF GFCTB0A2(4:2)
                                       TO WRK-MES-FIM-ADSAO-INDVD-A.
           MOVE DFIM-VGCIA-TARIF       OF GFCTB0A2(7:4)
                                       TO WRK-ANO-FIM-ADSAO-INDVD-A.

           IF (WRK-MES-FIM-ADSAO-INDVD EQUAL 01 OR 03 OR 05 OR 07 OR
                                             08 OR 10 OR 12) AND
               WRK-DIA-FIM-ADSAO-INDVD EQUAL 31
               GO TO 2420-99-FIM
           END-IF.

           IF (WRK-MES-FIM-ADSAO-INDVD EQUAL 04 OR 06 OR
                                             09 OR 11) AND
               WRK-DIA-FIM-ADSAO-INDVD EQUAL 30
               GO TO 2420-99-FIM
           END-IF.

           IF WRK-MES-FIM-ADSAO-INDVD EQUAL 02 AND
              WRK-DIA-FIM-ADSAO-INDVD EQUAL 29
               GO TO 2420-99-FIM
           END-IF.

           MOVE   WRK-ANO-FIM-ADSAO-INDVD TO WRK-ANO-AUX-C
           DIVIDE WRK-ANO-AUX-C           BY 4
                                  GIVING WRK-INTEIRO
                                  REMAINDER WRK-RESTO
           IF WRK-MES-FIM-ADSAO-INDVD EQUAL 02 AND
              WRK-RESTO               NOT EQUAL ZEROS AND
              WRK-DIA-FIM-ADSAO-INDVD EQUAL 28
               GO TO 2420-99-FIM
           END-IF.

           MOVE     WRK-MES-FIM-ADSAO-INDVD TO   WRK-MES-AUX-C
           SUBTRACT 1                       FROM WRK-MES-AUX-C
           MOVE     WRK-MES-AUX-C           TO   WRK-MES-FIM-ADSAO-INDVD

           EVALUATE WRK-MES-FIM-ADSAO-INDVD

           WHEN 00
                MOVE 31        TO WRK-DIA-FIM-ADSAO-INDVD
                MOVE 12        TO WRK-MES-FIM-ADSAO-INDVD
                MOVE WRK-ANO-FIM-ADSAO-INDVD TO WRK-ANO-AUX-C
                SUBTRACT 1     FROM WRK-ANO-AUX-C
                MOVE WRK-ANO-AUX-C           TO WRK-ANO-FIM-ADSAO-INDVD

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
           END-EVALUATE.

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
               IF CMTFWY4I-E-AMBIENTE EQUAL 'O'
                   MOVE 1318               TO CMTFWY4I-COD-MENSAGEM
               ELSE
                   MOVE 1179               TO CMTFWY4I-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 12                 TO CMTFWY4I-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO CMTFWY4I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY4I-SQLCA)
                                       TO CMTFWY4I-SQLCA
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
                  IF  CMTFWY4I-E-CLIE-CPFCNPJ         NOT EQUAL ZEROS
                      PERFORM 2432-ACESSAR-GFCTB0I0

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 2450-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 018         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 009

RJOVA           IF  WRK-1-VEZ-SEGMENTO EQUAL  'N'

                      PERFORM 2433-OBTER-SGTO-GESTAO

                    IF  SQLCODE        EQUAL ZEROS
                      PERFORM 2434-ACESSAR-GFCTB0D2

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 2450-OBTER-DADOS-GFCTB0C6
                      END-IF
                    END-IF
RJOVA           END-IF

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

RJOVA             IF  CMTFWY4I-E-TP-POSTO             EQUAL 1
                      PERFORM 2436-ACESSAR-GFCTB0D0

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 2450-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 017         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 017
RJOVA             IF  WRK-BANCO-POSTAL                EQUAL 's'
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
RJOVA             IF  CMTFWY4I-E-TP-POSTO             EQUAL 2
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
           MOVE CMTFWY4I-E-CLIE-CPFCNPJ
                                       TO CCGC-CPF-ST       OF GFCTB0I0.
           MOVE CMTFWY4I-E-CLIE-CPFCNPJ-FILIAL
                                       TO CFLIAL-CGC-ST     OF GFCTB0I0.
           MOVE CMTFWY4I-E-CLIE-CPFCNPJ-CTRL
                                       TO CCTRL-CPF-CGC-ST  OF GFCTB0I0.

           EXEC SQL
             SELECT MAX(DINIC_PRMSS_PCOTE)
             INTO
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
                   DINIC_PRMSS_PCOTE <= :GFCTB0I0.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W'      )
               IF CMTFWY4I-E-AMBIENTE EQUAL 'O'
                   MOVE 1324               TO CMTFWY4I-COD-MENSAGEM
               ELSE
                   MOVE 1184               TO CMTFWY4I-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 12                 TO CMTFWY4I-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO CMTFWY4I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY4I-SQLCA)
                                       TO CMTFWY4I-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -305
               GO  TO  2432-99-FIM
           END-IF.

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
                   DINIC_PRMSS_PCOTE  = :GFCTB0I0.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               IF CMTFWY4I-E-AMBIENTE EQUAL 'O'
                   MOVE 1324               TO CMTFWY4I-COD-MENSAGEM
               ELSE
                   MOVE 1184               TO CMTFWY4I-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 12                 TO CMTFWY4I-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO CMTFWY4I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY4I-SQLCA)
                                       TO CMTFWY4I-SQLCA
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
      *----------------------------------------------------------------*
RJOVA  2433-OBTER-SGTO-GESTAO          SECTION.
      *----------------------------------------------------------------*
      *
           MOVE  CMTFWY4I-E-CLIE-SEGMENTO
                                       TO CSGMTO-INIC-CLI   OF GFCTB0H3
                                          CSGMTO-FNAL-CLI   OF GFCTB0H3.
           MOVE  DPROCM-ATUAL          OF GFCTB0A1
                                       TO DINIC-VGCIA-ASSOC OF GFCTB0H3.

           MOVE  DPROCM-ATUAL          OF GFCTB0A1
                                       TO DFIM-VGCIA-ASSOC  OF GFCTB0H3.

           MOVE ZEROS                  TO CFUNC-EXCL-SIST   OF GFCTB0H3.
           MOVE ZEROS                  TO WRK-CSGMTO-GSTAO-TARIF-I.
           MOVE 999                    TO WRK-CSGMTO-GSTAO-TARIF-F.

           EXEC SQL
            SELECT A.CSGMTO_GSTAO_TARIF
            INTO   :GFCTB0H3.CSGMTO-GSTAO-TARIF
            FROM   DB2PRD.TASSOC_SGMTO_CLI    AS A,
                   DB2PRD.TTPO_SGMTO_GSTAO    AS B
            WHERE  A.CSGMTO_GSTAO_TARIF  = B.CSGMTO_GSTAO_TARIF
              AND (A.CSGMTO_GSTAO_TARIF >= :WRK-CSGMTO-GSTAO-TARIF-I
              AND  A.CSGMTO_GSTAO_TARIF <= :WRK-CSGMTO-GSTAO-TARIF-F)
              AND  A.DINIC_VGCIA_ASSOC  <= :GFCTB0H3.DINIC-VGCIA-ASSOC
              AND  A.DFIM_VGCIA_ASSOC   >= :GFCTB0H3.DFIM-VGCIA-ASSOC
              AND (A.CSGMTO_INIC_CLI    <= :GFCTB0H3.CSGMTO-INIC-CLI
              AND  A.CSGMTO_FNAL_CLI    >= :GFCTB0H3.CSGMTO-FNAL-CLI)
              AND  A.CFUNC_EXCL_SIST     = :GFCTB0H3.CFUNC-EXCL-SIST
              AND  B.CTPO_PSSOA     IN ('A' , 'F', 'J')
              FETCH FIRST 1 ROWS ONLY
           END-EXEC.

           IF  SQLCODE               NOT EQUAL ZEROS AND +100  AND -811
            OR SQLWARN0                EQUAL     'W'
               IF CMTFWY4I-E-AMBIENTE EQUAL 'O'
                   MOVE 1318                TO CMTFWY4I-COD-MENSAGEM
               ELSE
                   MOVE 1198                TO CMTFWY4I-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 12                      TO CMTFWY4I-COD-RETORNO
               MOVE GFCTG3-DESC-MSG         TO CMTFWY4I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY4I-SQLCA)
                                            TO CMTFWY4I-SQLCA
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
RJOVA  2433-99-FIM.                    EXIT.
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

RJOVA      MOVE CSGMTO-GSTAO-TARIF     OF GFCTB0H3
                                       TO CSGMTO-GSTAO-TARIF
                                                            OF GFCTB0D2.

           EXEC SQL
             SELECT MAX(DINIC_PRMSS_PCOTE)
             INTO
                   :GFCTB0D2.DINIC-PRMSS-PCOTE
             FROM   DB2PRD.PRMSS_GRP_SGMTO
             WHERE
              CSERVC_TARIF       = :GFCTB0D2.CSERVC-TARIF       AND
              CAGPTO_CTA         = :GFCTB0D2.CAGPTO-CTA         AND
              CSGMTO_GSTAO_TARIF = :GFCTB0D2.CSGMTO-GSTAO-TARIF AND
              DINIC_PRMSS_PCOTE <= :GFCTB0D2.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W'      )
               IF CMTFWY4I-E-AMBIENTE EQUAL 'O'
                   MOVE 1328               TO CMTFWY4I-COD-MENSAGEM
               ELSE
                   MOVE 1190               TO CMTFWY4I-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 12                 TO CMTFWY4I-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO CMTFWY4I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY4I-SQLCA)
                                       TO CMTFWY4I-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -305
               GO  TO  2434-99-FIM
           END-IF.

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
                  DINIC_PRMSS_PCOTE  = :GFCTB0D2.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               IF CMTFWY4I-E-AMBIENTE EQUAL 'O'
                   MOVE 1328               TO CMTFWY4I-COD-MENSAGEM
               ELSE
                   MOVE 1190               TO CMTFWY4I-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 12                 TO CMTFWY4I-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO CMTFWY4I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY4I-SQLCA)
                                       TO CMTFWY4I-SQLCA
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
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO GENCIA.*
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
           MOVE WRK-CDEPDC             TO CDEPDC            OF GFCTB0C7.

           EXEC SQL
             SELECT MAX(DINIC_PRMSS_PCOTE)
             INTO
                   :GFCTB0C7.DINIC-PRMSS-PCOTE
             FROM   DB2PRD.PRMSS_GRP_DEPDC
             WHERE
               CSERVC_TARIF       = :GFCTB0C7.CSERVC-TARIF      AND
               CAGPTO_CTA         = :GFCTB0C7.CAGPTO-CTA        AND
              (CDEPDC             = 0                           OR
               CDEPDC             = :GFCTB0C7.CDEPDC)           AND
               DINIC_PRMSS_PCOTE <= :GFCTB0C7.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W'      )
               IF CMTFWY4I-E-AMBIENTE EQUAL 'O'
                   MOVE 1329               TO CMTFWY4I-COD-MENSAGEM
               ELSE
                   MOVE 1191               TO CMTFWY4I-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 12                 TO CMTFWY4I-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO CMTFWY4I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY4I-SQLCA)
                                       TO CMTFWY4I-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -305
               GO  TO  2435-99-FIM
           END-IF.

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
                   DINIC_PRMSS_PCOTE  = :GFCTB0C7.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               IF CMTFWY4I-E-AMBIENTE EQUAL 'O'
                   MOVE 1329               TO CMTFWY4I-COD-MENSAGEM
               ELSE
                   MOVE 1191               TO CMTFWY4I-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 12                 TO CMTFWY4I-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO CMTFWY4I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY4I-SQLCA)
                                       TO CMTFWY4I-SQLCA
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
           MOVE WRK-CDEPDC             TO CDEPDC            OF GFCTB0D0.
           MOVE CMTFWY4I-E-CD-COML-POSTO-SERVC
                                       TO CPOSTO-SERVC      OF GFCTB0D0.

           EXEC SQL
             SELECT MAX(DINIC_PRMSS_PCOTE)
             INTO
                   :GFCTB0D0.DINIC-PRMSS-PCOTE
             FROM   DB2PRD.PRMSS_GRP_PAB
             WHERE
               CSERVC_TARIF       = :GFCTB0D0.CSERVC-TARIF      AND
               CAGPTO_CTA         = :GFCTB0D0.CAGPTO-CTA        AND
               CDEPDC             = :GFCTB0D0.CDEPDC            AND
              (CPOSTO_SERVC       = 0                           OR
               CPOSTO_SERVC       = :GFCTB0D0.CPOSTO-SERVC)     AND
               DINIC_PRMSS_PCOTE <= :GFCTB0D0.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W'      )
               IF CMTFWY4I-E-AMBIENTE EQUAL 'O'
                   MOVE 1330               TO CMTFWY4I-COD-MENSAGEM
               ELSE
                   MOVE 1192               TO CMTFWY4I-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 12                 TO CMTFWY4I-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO CMTFWY4I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY4I-SQLCA)
                                       TO CMTFWY4I-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -305
               GO  TO  2436-99-FIM
           END-IF.

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
                   DINIC_PRMSS_PCOTE  = :GFCTB0D0.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               IF CMTFWY4I-E-AMBIENTE EQUAL 'O'
                   MOVE 1330               TO CMTFWY4I-COD-MENSAGEM
               ELSE
                   MOVE 1192               TO CMTFWY4I-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 12                 TO CMTFWY4I-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO CMTFWY4I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY4I-SQLCA)
                                       TO CMTFWY4I-SQLCA
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
           MOVE WRK-CDEPDC             TO CDEPDC            OF GFCTB0D1.
           MOVE CMTFWY4I-E-CD-COML-POSTO-SERVC
                                       TO CPOSTO-SERVC      OF GFCTB0D1.

           EXEC SQL
             SELECT MAX(DINIC_PRMSS_PCOTE)
             INTO
                   :GFCTB0D1.DINIC-PRMSS-PCOTE
             FROM   DB2PRD.PRMSS_GRP_PSTAL
             WHERE
               CSERVC_TARIF       = :GFCTB0D1.CSERVC-TARIF      AND
               CAGPTO_CTA         = :GFCTB0D1.CAGPTO-CTA        AND
              (CDEPDC             = 0                           OR
               CDEPDC             = :GFCTB0D1.CDEPDC            AND
               CPOSTO_SERVC       = :GFCTB0D1.CPOSTO-SERVC)     AND
               DINIC_PRMSS_PCOTE <= :GFCTB0D1.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W'      )
               IF CMTFWY4I-E-AMBIENTE EQUAL 'O'
                   MOVE 1331               TO CMTFWY4I-COD-MENSAGEM
               ELSE
                   MOVE 1193               TO CMTFWY4I-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 12                 TO CMTFWY4I-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO CMTFWY4I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY4I-SQLCA)
                                       TO CMTFWY4I-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -305
               GO  TO  2437-99-FIM
           END-IF.

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
                   DINIC_PRMSS_PCOTE  = :GFCTB0D1.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               IF CMTFWY4I-E-AMBIENTE EQUAL 'O'
                   MOVE 1331               TO CMTFWY4I-COD-MENSAGEM
               ELSE
                   MOVE 1193               TO CMTFWY4I-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 12                 TO CMTFWY4I-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO CMTFWY4I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY4I-SQLCA)
                                       TO CMTFWY4I-SQLCA
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

           MOVE CMTFWY4I-E-AMBIENTE    TO GFCTRK-TPO-AMBIENTE.
           MOVE WRK-CDEPDC-S           TO GFCTRK-CJUNC-DEPDC.

           MOVE 'GFCT5564'             TO WRK-MODULO.

           CALL WRK-MODULO             USING
                                             GFCTRK-AREA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               IF CMTFWY4I-E-AMBIENTE EQUAL 'O'
                   MOVE 1332               TO CMTFWY4I-COD-MENSAGEM
               ELSE
                   MOVE 1194               TO CMTFWY4I-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 12                 TO CMTFWY4I-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO CMTFWY4I-MENSAGEM
               MOVE SPACES             TO CMTFWY4I-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  GFCTRK-COD-RETORNO      EQUAL 01 OR 02 OR 99
               IF CMTFWY4I-E-AMBIENTE EQUAL 'O'
                   MOVE 1333               TO CMTFWY4I-COD-MENSAGEM
               ELSE
                   MOVE 1195               TO CMTFWY4I-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 12                 TO CMTFWY4I-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO CMTFWY4I-MENSAGEM
               MOVE SPACES             TO CMTFWY4I-SQLCA
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
           MOVE GFCTRK-CMUN-RURAL      TO WRK-GFCTRK-CMUN-RURAL.
           MOVE WRK-CMUN-RURAL         TO CMUN-IBGE         OF GFCTB0C9.

           EXEC SQL
             SELECT MAX(DINIC_PRMSS_PCOTE)
             INTO
                   :GFCTB0C9.DINIC-PRMSS-PCOTE
             FROM   DB2PRD.PRMSS_GRP_MUN
             WHERE
               CSERVC_TARIF       = :GFCTB0C9.CSERVC-TARIF      AND
               CAGPTO_CTA         = :GFCTB0C9.CAGPTO-CTA        AND
               CMUN_IBGE          = :GFCTB0C9.CMUN-IBGE         AND
               DINIC_PRMSS_PCOTE <= :GFCTB0C9.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W'      )
               IF CMTFWY4I-E-AMBIENTE EQUAL 'O'
                   MOVE 1334               TO CMTFWY4I-COD-MENSAGEM
               ELSE
                   MOVE 1196               TO CMTFWY4I-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 12                 TO CMTFWY4I-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO CMTFWY4I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY4I-SQLCA)
                                       TO CMTFWY4I-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -305
               GO  TO  2439-99-FIM
           END-IF.

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
                   DINIC_PRMSS_PCOTE  = :GFCTB0C9.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               IF CMTFWY4I-E-AMBIENTE EQUAL 'O'
                   MOVE 1334               TO CMTFWY4I-COD-MENSAGEM
               ELSE
                   MOVE 1196               TO CMTFWY4I-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 12                 TO CMTFWY4I-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO CMTFWY4I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY4I-SQLCA)
                                       TO CMTFWY4I-SQLCA
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
             SELECT MAX(DINIC_PRMSS_PCOTE)
             INTO
                   :GFCTB0D3.DINIC-PRMSS-PCOTE
             FROM   DB2PRD.PRMSS_GRP_UF
             WHERE
               CSERVC_TARIF       = :GFCTB0D3.CSERVC-TARIF      AND
               CAGPTO_CTA         = :GFCTB0D3.CAGPTO-CTA        AND
               CSGL_UF            = :GFCTB0D3.CSGL-UF           AND
               DINIC_PRMSS_PCOTE <= :GFCTB0D3.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W'      )
               IF CMTFWY4I-E-AMBIENTE EQUAL 'O'
                   MOVE 1335               TO CMTFWY4I-COD-MENSAGEM
               ELSE
                   MOVE 1197               TO CMTFWY4I-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 12                 TO CMTFWY4I-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO CMTFWY4I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY4I-SQLCA)
                                       TO CMTFWY4I-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -305
               GO  TO  2440-99-FIM
           END-IF.

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
                   DINIC_PRMSS_PCOTE  = :GFCTB0D3.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               IF CMTFWY4I-E-AMBIENTE EQUAL 'O'
                   MOVE 1335               TO CMTFWY4I-COD-MENSAGEM
               ELSE
                   MOVE 1197               TO CMTFWY4I-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 12                 TO CMTFWY4I-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO CMTFWY4I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY4I-SQLCA)
                                       TO CMTFWY4I-SQLCA
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
               IF CMTFWY4I-E-AMBIENTE EQUAL 'O'
                   MOVE 1336               TO CMTFWY4I-COD-MENSAGEM
               ELSE
                   MOVE 1198               TO CMTFWY4I-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 12                 TO CMTFWY4I-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO CMTFWY4I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY4I-SQLCA)
                                       TO CMTFWY4I-SQLCA
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
                   MOVE WRK-MES-INI-ADSAO-INDVD TO WRK-MES-AUX-C
                   ADD 1                    TO WRK-MES-AUX-C
                   MOVE WRK-MES-AUX-C       TO WRK-MES-INI-ADSAO-INDVD
                   IF  WRK-MES-INI-ADSAO-INDVD
                                            GREATER 12
                       MOVE 01              TO WRK-MES-INI-ADSAO-INDVD
                       MOVE WRK-ANO-INI-ADSAO-INDVD TO WRK-ANO-AUX-C
                       ADD  1               TO WRK-ANO-AUX-C
                       MOVE WRK-ANO-AUX-C   TO WRK-ANO-INI-ADSAO-INDVD
                   END-IF
               END-IF
           ELSE
               MOVE WRK-DT-PROCM-ATUAL      TO WRK-DT-INI-ADSAO-INDVD
               MOVE 01                      TO WRK-DIA-INI-ADSAO-INDVD
               MOVE WRK-MES-INI-ADSAO-INDVD TO WRK-MES-AUX-C
               ADD 1                        TO WRK-MES-AUX-C
               MOVE WRK-MES-AUX-C           TO WRK-MES-INI-ADSAO-INDVD
               IF  WRK-MES-INI-ADSAO-INDVD  GREATER 12
                   MOVE 01                  TO WRK-MES-INI-ADSAO-INDVD
                   MOVE WRK-ANO-INI-ADSAO-INDVD TO WRK-ANO-AUX-C
                   ADD  1                   TO WRK-ANO-AUX-C
                   MOVE WRK-ANO-AUX-C       TO WRK-ANO-INI-ADSAO-INDVD
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

           ADD +1                      TO IND-1.

           IF  IND-1                   GREATER +150
               MOVE +150               TO IND-1
               MOVE 'S'                TO WRK-FIM-CURSOR

               IF CMTFWY4I-E-AMBIENTE EQUAL 'O'
                   MOVE 1338               TO CMTFWY4I-COD-MENSAGEM
               ELSE
                   MOVE 1200               TO CMTFWY4I-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 00                 TO CMTFWY4I-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO CMTFWY4I-MENSAGEM
               MOVE ZEROS              TO RETURN-CODE
               GO TO 2470-99-FIM
           END-IF.

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                TO CMTFWY4I-S-CD-CESTA-PERM(IND-1).
           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8 (1:40)
                         TO CMTFWY4I-S-DS-CESTA-PERM(IND-1).
           MOVE WRK-DIA-INI-ADSAO-INDVD-A
                         TO CMTFWY4I-S-DT-INICIO-VGCIA(IND-1)(1:2).
           MOVE WRK-MES-INI-ADSAO-INDVD-A
                         TO CMTFWY4I-S-DT-INICIO-VGCIA(IND-1)(4:2).
           MOVE WRK-ANO-INI-ADSAO-INDVD-A
                         TO CMTFWY4I-S-DT-INICIO-VGCIA(IND-1)(7:4).
           MOVE '.'      TO CMTFWY4I-S-DT-INICIO-VGCIA(IND-1)(3:1)
                            CMTFWY4I-S-DT-INICIO-VGCIA(IND-1)(6:1).
           MOVE WRK-DIA-FIM-ADSAO-INDVD-A
                         TO CMTFWY4I-S-DT-FIM-VGCIA(IND-1)(1:2).
           MOVE WRK-MES-FIM-ADSAO-INDVD-A
                         TO CMTFWY4I-S-DT-FIM-VGCIA(IND-1)(4:2).
           MOVE WRK-ANO-FIM-ADSAO-INDVD-A
                         TO CMTFWY4I-S-DT-FIM-VGCIA(IND-1)(7:4).
           MOVE '.'      TO CMTFWY4I-S-DT-FIM-VGCIA(IND-1)(3:1)
                            CMTFWY4I-S-DT-FIM-VGCIA(IND-1)(6:1).

           PERFORM 2310-ACESSAR-GFCTB0A2.
           MOVE WRK-CESTA-CELULAR
                                TO CMTFWY4I-S-CD-INDCD-CLULR(IND-1).

           PERFORM 2300-LER-CURSOR.

      *----------------------------------------------------------------*
       2470-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * FECHAR CURSOR                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-FECHAR-CURSOR              SECTION.
      *----------------------------------------------------------------*

           IF   WRK-NUM-CURSOR EQUAL 01
                EXEC SQL
                     CLOSE CSR01-GFCTB0C6-JN
                END-EXEC
           ELSE
                EXEC SQL
                     CLOSE CSR02-GFCTB0C6-JN
                END-EXEC
           END-IF.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               IF CMTFWY4I-E-AMBIENTE EQUAL 'O'
                   MOVE 1313               TO CMTFWY4I-COD-MENSAGEM
               ELSE
                   MOVE 1174               TO CMTFWY4I-COD-MENSAGEM
               END-IF
               PERFORM 1200-OBTER-DESC-MSG
               MOVE 12                 TO CMTFWY4I-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO CMTFWY4I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY4I-SQLCA)
                                       TO CMTFWY4I-SQLCA
               MOVE 12                 TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ORDENAR OCORRENCIAS DE SAIDA CONFORME SOLICITADO               *
      ******************************************************************
      *----------------------------------------------------------------*
       2600-ORDENAR-OCORRENCIAS        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-NUM-CURSOR      EQUAL 02
               MOVE ZEROS          TO           IND-2
               MOVE CMTFWY4I-SAIDA TO           WRK-CMTFWY4I-SAIDA
               PERFORM             VARYING IND-1 FROM IND-1 BY -1
                                   UNTIL   IND-1 EQUAL     ZEROS
                    ADD  1         TO           IND-2
                    MOVE CMTFWY4I-S-REG-LISTA(IND-1)
                                   TO WRK-CMTFWY4I-S-REG-LISTA(IND-2)
               END-PERFORM
               MOVE WRK-CMTFWY4I-SAIDA   TO     CMTFWY4I-SAIDA
           ELSE
               MOVE IND-1          TO           IND-2
           END-IF.

           MOVE IND-2              TO CMTFWY4I-S-QTD-LISTA.
           MOVE CMTFWY4I-S-CD-CESTA-PERM(1)
                                   TO CMTFWY4I-I-CD-CESTA-PERM
           MOVE CMTFWY4I-S-CD-CESTA-PERM(IND-2)
                                   TO CMTFWY4I-F-CD-CESTA-PERM.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
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
