      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT6046.
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
      *    PROGRAMA....:   GFCT6046                                    *
      *    PROGRAMADOR.:   LUIZ MOREIRA XAVIER - PROCWORK/GP50         *
      *    ANALISTA....:   LUIZ MOREIRA XAVIER - PROCWORK/GP50         *
      *    DATA........:   SETEMBRO/2015                               *
      *----------------------------------------------------------------*
      *                                                                *
      *    OBJETIVO....:   MODULO PARA INCLUSAO DE ADESAO INDIVIDUAL   *
      *                    A PACOTES CESTA CELULAR                     *
      *                                                                *
      *----------------------------------------------------------------*
      *                                                                *
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                       INCLUDE/BOOK      *
      *                    DB2PRD.PARM_DATA_PROCM      GFCTB0A1        *
      *                    DB2PRD.PARM_SERVC_TARIF     GFCTB0A2        *
      *                    DB2PRD.PRMSS_ADSAO_PCOTE    GFCTB0C6        *
      *                    DB2PRD.PRMSS_GRP_DEPDC      GFCTB0C7        *
      *                    DB2PRD.PRMSS_GRP_MUN        GFCTB0C9        *
      *                    DB2PRD.PRMSS_GRP_PAB        GFCTB0D0        *
      *                    DB2PRD.PRMSS_GRP_PSTAL      GFCTB0D1        *
      *                    DB2PRD.PRMSS_GRP_SGMTO      GFCTB0D2        *
      *                    DB2PRD.PRMSS_GRP_UF         GFCTB0D3        *
      *                    DB2PRD.SERVC_TARIF_PRINC    GFCTB0D8        *
      *                    DB2PRD.TPRMSS_GRP_CLI       GFCTB0I0        *
      *                    DB2PRD.V01CLIENTE_AGENCIA   CLIEV004        *
      *                    DB2PRD.V01CTA_POUPANCA      CLIEV007        *
      *                    DB2PRD.V01CTA_CORRENTE      CLIEV008        *
      *                    DB2PRD.ADSAO_INDVD_PCOTE    GFCTB009        *
      *                    DB2PRD.TADSAO_INDVD_CLULR   GFCTB0M2        *
      *                    DB2PRD.TOPER_FONE_CREDT     GFCTB0M4        *
      *                                                                *
      *----------------------------------------------------------------*
      *                                                                *
      *    BOOK'S......:                                               *
      *       CMTFWY6I - ENTRADA E SAIDA - INC MODULO CORPORATIVO      *
      *                  GFCT6046                                      *
      *       I#GFCTG2 - ENTRADA - OBTER DESCRICAO DE MENSAGEM.        *
      *       I#GFCTG3 - SAIDA   - OBTER DESCRICAO DE MENSAGEM.        *
      *       I#GFCTJM - ENTRADA E SAIDA - OBTER COD SEG GESTAO TARIFA *
      *       I#GFCTRK - ENTRADA E SAIDA - OBTER COD UF E MUNICIPIO DA *
      *                  AGENCIA.                                      *
      *       GFCTWAN7 - AREA DE COMUNICACAO COM GFCT5307 - SAIDA      *
      *                                                                *
      *----------------------------------------------------------------*
      *                                                                *
      *    MODULOS.....:                                               *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    GFCT5527 - OBTER CPF/CNPJ/POSTO DE SERVICO.                 *
      *    GFCT5534 - OBTER CODIGO SEGMENTO GESTAO TARIFA.             *
      *    GFCT5564 - OBTER COD DA UF E MUNICIPIO DA AGENCIA.          *
      *    GFCT5507 - VALIDAR NUMERO DO CELULAR NA ADESAO.             *
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
       01  FILLER                      PIC  X(050)         VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       01  WRK-NUM04-A                 PIC  X(04)          VALUE ZEROS.
       01  FILLER            REDEFINES WRK-NUM04-A.
           05 FILLER                   PIC  X(01).
           05 WRK-NUM03-N              PIC  9(03).

       01  WRK-NUM09-N                 PIC  9(09)          VALUE ZEROS.
       01  FILLER            REDEFINES WRK-NUM09-N.
           05 FILLER                   PIC  X(02).
           05 WRK-NUM07-N              PIC  9(07).

       01  WRK-NUM11-N                 PIC  9(11)          VALUE ZEROS.
       01  FILLER            REDEFINES WRK-NUM11-N.
           05 FILLER                   PIC  X(02).
           05 WRK-NUM09-D              PIC  9(01).
           05 WRK-NUM08-D              PIC  9(01).
           05 FILLER                   PIC  X(07).

       01  WRK-DT-PROC-A1              PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-PROC-A1-R            REDEFINES WRK-DT-PROC-A1.
           03  WRK-ANO-PROC-A1         PIC  X(004).
           03  WRK-MES-PROC-A1         PIC  X(002).
           03  WRK-DIA-PROC-A1         PIC  X(002).


       01  WRK-MODULO                  PIC  X(008)         VALUE SPACES.

       01  WRK-TIPO-PESSOA-CLIENTE     PIC  X(001)         VALUE SPACES.

       01  WRK-TIPO-CONTA-CLIENTE      PIC S9(001) COMP-3  VALUE ZEROS.

       01  WRK-RESTO                   PIC  9(003) COMP-3  VALUE ZEROS.
       01  WRK-INTEIRO                 PIC  9(004) COMP-3  VALUE ZEROS.

       01  WRK-FIM-CURSOR              PIC  X(001)         VALUE 'N'.
       01  WRK-FIM-AGRUPAMENTO         PIC  X(001)         VALUE SPACES.

       01  WRK-TARIFA-VALIDA           PIC  X(001)         VALUE SPACES.

       01  WRK-1-VEZ-SEGMENTO          PIC  X(001)         VALUE 'N'.
       01  WRK-1-VEZ-CNPJ-CPF          PIC  X(001)         VALUE 'N'.
       01  WRK-1-VEZ-UF-MUNIC          PIC  X(001)         VALUE 'N'.

       01  WRK-EXISTE-COMPONENTE       PIC  X(001)         VALUE 'N'.
       01  WRK-EXISTE-AGRUPAMENTO      PIC  X(001)         VALUE SPACES.
       01  WRK-AGRUPAMENTO-VALIDO      PIC  X(001)         VALUE SPACES.
       01  WRK-PROX-AGRUPAMENTO        PIC  9(003)         VALUE ZEROS.

       01  WRK-CINDCD-ADSAO-INDVD      PIC  X(001)         VALUE 'I'.
       01  WRK-CINDCD-ADSAO-AMBAS      PIC  X(001)         VALUE 'A'.

       01  WRK-SEQ-AGPTO               PIC S9(009) COMP-3  VALUE ZEROS.

       01  WRK-DINIC-PRMSS-PCOTE       PIC  X(010)         VALUE SPACES.

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
       01  FILLER                      REDEFINES WRK-DT-INI-ADSAO-INDVD.
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
       01  FILLER                      REDEFINES WRK-DT-FIM-ADSAO-INDVD.
           03  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       PIC  9(006).
           03  FILLER                  REDEFINES
                                             WRK-AAAAMM-FIM-ADSAO-INDVD.
               05  WRK-ANO-FIM-ADSAO-INDVD
                                       PIC  9(004).
               05  WRK-MES-FIM-ADSAO-INDVD
                                       PIC  9(002).
           03  WRK-DIA-FIM-ADSAO-INDVD PIC  9(002).

       01  WRK-HINCL-REG               PIC  X(026)         VALUE SPACES.

       01  WRK-CESTA-CELULAR           PIC  X(001)         VALUE SPACES.

       01  WRK-MES                     PIC  9(002) COMP-3  VALUE ZEROS.
       01  WRK-ANO                     PIC  9(004) COMP-3  VALUE ZEROS.

       01  WRK-DATA-DB2.
           03  WRK-DIA-DB2             PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-MES-DB2             PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-ANO-DB2             PIC  9(004)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*  AREA PARA MENSAGEM INTERNA  *'.
      *----------------------------------------------------------------*

       01  WRK-MSG-SCCE9100.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA AO MODULO '.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA PARA MENSAGENS MODULO GFCT5523 - MENSAGENS *'.
      *----------------------------------------------------------------*

       01  WRK-MSG-1                    PIC  X(070)        VALUE
           'NAO FOI POSSIVEL OBTER A MSG NO MODULO GFCT5523'.

       01  WRK-MSG-2                    PIC  X(070)        VALUE
           'PROBLEMA NO ACESSO A BASE DE MENSAGENS'.

       01  WRK-MSG-3                    PIC  X(070)        VALUE
           'MENSAGEM NAO ENCONTRADA NA BASE DE MENSAGENS'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5527 *'.
      *----------------------------------------------------------------*

       01  WRK-5527-AREA-ENTRADA.
           05  WRK-5527-AMBIENTE       PIC  X(001)         VALUE SPACES.
           05  WRK-5527-CHAMADOR       PIC  X(008)         VALUE SPACES.
           05  WRK-5527-AGENCIA-CLIENTE
                                       PIC  9(005)         VALUE ZEROS.
           05  WRK-5527-CONTA-CLIENTE  PIC  9(013)         VALUE ZEROS.

       01  WRK-5527-AREA-SAIDA.
           05  WRK-5527-COD-RETORNO    PIC  9(002)         VALUE ZEROS.
           05  WRK-5527-COD-SQL-ERRO   PIC S9(003)         VALUE ZEROS.
           05  WRK-5527-COD-MSG-GFCT   PIC  9(004)         VALUE ZEROS.
           05  WRK-5527-DESC-MSG       PIC  X(070)         VALUE SPACES.
           05  WRK-5527-QTD-OCORR      PIC  9(005)         VALUE ZEROS.
           05  WRK-5527-DADOS-RETORNO.
ST25X6*        10  WRK-5527-CCGC-CPF   PIC  9(009)         VALUE ZEROS.
ST25X6         10  WRK-5527-CCGC-CPF   PIC  X(009)         VALUE SPACES.
ST25X6*        10  WRK-5527-CFLIAL-CGC PIC  9(004)         VALUE ZEROS.
ST25X6         10  WRK-5527-CFLIAL-CGC PIC  X(004)         VALUE SPACES.
               10  WRK-5527-CCTRL-CPF-CGC
                                       PIC  9(002)         VALUE ZEROS.
               10  WRK-5527-CPOSTO-SERVC
                                       PIC  9(003)         VALUE ZEROS.
               10  WRK-5527-CSGMTO-CLI PIC  9(003)         VALUE ZEROS.

       01  WRK-IO-PCB.
           03  WRK-IO-TERM             PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  WRK-IO-STA              PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(012)         VALUE SPACES.
           03  WRK-IO-MODNAME          PIC  X(008)         VALUE SPACES.

       01  WRK-ALT-PCB.
           03  WRK-ALT-PCBNAME         PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  WRK-ALT-STATUS          PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(012)         VALUE SPACES.
           03  WRK-ALT-MODNAME         PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5307 - SAIDA *'.
      *----------------------------------------------------------------*

       COPY 'GFCTWAN7'.

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

           EXEC SQL
            INCLUDE GFCTB0M2
           END-EXEC.

           EXEC SQL
            INCLUDE GFCTB0M4
           END-EXEC.

           EXEC SQL
            INCLUDE GFCTB0N7
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DECLARACAO DE CURSOR *'.
      *----------------------------------------------------------------*

           EXEC SQL
            DECLARE
                     CSR01-GFCTB029 CURSOR FOR
             SELECT
                     DISTINCT(CTARIF_COMPO_PCOTE)
               FROM
                     DB2PRD.COMPS_TARIF_PCOTE
              WHERE
                     CTARIF_PCOTE       = :GFCTB029.CTARIF-PCOTE
           ORDER BY
                     CTARIF_COMPO_PCOTE
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-CMTFWY6I.
       COPY 'CMTFWY6I'.

      *================================================================*
       PROCEDURE                       DIVISION USING LNK-CMTFWY6I.
      *================================================================*

      ******************************************************************
      * ROTINA PRINCIPAL.                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           IF  CMTFWY6I-E-AMBIENTE     EQUAL 'B' OR 'b'
               CALL 'POOL1050'
           END-IF.

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR-REGISTROS.

           PERFORM 9999-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA INICIALIZACAO DO PROGRAMA.                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 1100-CONSISTIR-DADOS.

           PERFORM 1200-OBTER-DATA-PROCESSAMENTO.

RJOVA *---   DESVIO PARA NAO CHECAR A PERMISSAO NO GFCT SERA
RJOVA *---   A DO GOTF VALIDA

RJOVA      IF CMTFWY6I-E-PERMIS-GOTF
RJOVA                             NOT EQUAL 'S'
              PERFORM 1300-CONSISTE-PACOTE
RJOVA      END-IF.

           PERFORM 1400-VALIDAR-CONTA.

           PERFORM 1500-CONSISTE-AGENCIA-CONTA.

RJOVA *---   DESVIO PARA NAO REFAZER VALIDACOES REALIZADAS JA NO GOTF

RJOVA      IF CMTFWY6I-E-PERMIS-GOTF
                                  NOT EQUAL 'S'
              PERFORM 1600-VALIDAR-OPERADORA
RJOVA      END-IF.

           MOVE CMTFWY6I-E-NUMERO TO WRK-NUM11-N.

RJOVA *---   DESVIO PARA NAO REFAZER VALIDACOES REALIZADAS JA NO GOTF
RJOVA      IF CMTFWY6I-E-PERMIS-GOTF
                                  NOT EQUAL 'S'
             IF WRK-DT-PROC-A1    NOT LESS 20120729
              PERFORM 3105-VERIFICA-PERMISSAO-ADESAO
              PERFORM 1700-VALIDAR-CELULAR
             ELSE
                IF WRK-NUM09-D    EQUAL ZEROS
                   PERFORM 1700-VALIDAR-CELULAR
                ELSE
                 MOVE 08          TO CMTFWY6I-COD-RETORNO
                 MOVE '1858'      TO GFCTG2-COD-MSG
                 MOVE 1858        TO CMTFWY6I-COD-MENSAGEM
                 PERFORM 9800-OBTER-DESC-MSG-S-FIN
                 MOVE GFCTG3-DESC-MSG
                                  TO CMTFWY6I-MENSAGEM
                 MOVE SQLCA(1:LENGTH
                                  OF CMTFWY6I-SQLCA)
                                  TO CMTFWY6I-SQLCA
                 MOVE 4           TO RETURN-CODE
                 PERFORM 9999-FINALIZAR
                END-IF
             END-IF
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONSISTENCIA DOS DADOS DE ENTRADA.                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF  CMTFWY6I-E-AMBIENTE     EQUAL 'o'
               MOVE 'O'                TO CMTFWY6I-E-AMBIENTE
           END-IF.

           IF  CMTFWY6I-E-AMBIENTE     EQUAL 'b'
               MOVE 'B'                TO CMTFWY6I-E-AMBIENTE
           END-IF.

           IF (CMTFWY6I-E-CD-COML-AGENCIA        NOT NUMERIC)      OR
              (CMTFWY6I-E-CD-COML-AGENCIA        EQUAL ZEROS)      OR
              (CMTFWY6I-E-CD-COML-CONTA          NOT NUMERIC)      OR
              (CMTFWY6I-E-CD-COML-CONTA          EQUAL ZEROS)      OR
              (CMTFWY6I-E-CD-CESTA-PERM          NOT NUMERIC)      OR
              (CMTFWY6I-E-DDD                    NOT NUMERIC)      OR
              (CMTFWY6I-E-NUMERO                 NOT NUMERIC)      OR
              (CMTFWY6I-E-OPERADORA              NOT NUMERIC)      OR
              (CMTFWY6I-E-AMBIENTE               NOT EQUAL 'O' AND 'B')
               MOVE 'B'                TO CMTFWY6I-E-AMBIENTE

               MOVE 08                 TO CMTFWY6I-COD-RETORNO
      *
      *--- DADOS ENVIADOS AO MODULO GFCT6050 INCONSISTENTES
      *
               MOVE '1501'             TO GFCTG2-COD-MSG
               MOVE 1501               TO CMTFWY6I-COD-MENSAGEM

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SPACES             TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           PERFORM 1110-CONSISTIR-DADOS-CELULAR.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONSISTENCIA DOS DADOS DO CELULAR                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-CONSISTIR-DADOS-CELULAR    SECTION.
      *----------------------------------------------------------------*

           IF  CMTFWY6I-E-DDD          EQUAL ZEROS AND
               CMTFWY6I-E-NUMERO       EQUAL ZEROS AND
               CMTFWY6I-E-OPERADORA    EQUAL ZEROS
               MOVE 08                 TO CMTFWY6I-COD-RETORNO
      *
      *--- PACOTE CESTA CELULAR. NUMERO CELULAR OBRIGATORIO.
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1802'         TO GFCTG2-COD-MSG
                   MOVE 1802           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1596'         TO GFCTG2-COD-MSG
                   MOVE 1596           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG     TO CMTFWY6I-MENSAGEM
               MOVE SPACES              TO CMTFWY6I-SQLCA
               MOVE 12                  TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  CMTFWY6I-E-DDD          EQUAL ZEROS
               MOVE 08                 TO CMTFWY6I-COD-RETORNO
      *
      *--- DADOS ENVIADOS NO CODIGO DDD INVALIDOS
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1714'         TO GFCTG2-COD-MSG
                   MOVE 1714           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1504'         TO GFCTG2-COD-MSG
                   MOVE 1504           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG     TO CMTFWY6I-MENSAGEM
               MOVE SPACES              TO CMTFWY6I-SQLCA
               MOVE 12                  TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  CMTFWY6I-E-NUMERO       EQUAL ZEROS
               MOVE 08                  TO CMTFWY6I-COD-RETORNO
      *
      *--- DADOS ENVIADOS NO NUMERO CELULAR INVALIDOS
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1715'         TO GFCTG2-COD-MSG
                   MOVE 1715           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1505'         TO GFCTG2-COD-MSG
                   MOVE 1505           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG     TO CMTFWY6I-MENSAGEM
               MOVE SPACES              TO CMTFWY6I-SQLCA
               MOVE 12                  TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  CMTFWY6I-E-OPERADORA    EQUAL ZEROS
               MOVE 08                 TO CMTFWY6I-COD-RETORNO
      *
      *--- DADOS ENVIADOS NO CODIGO DA OPERADORA INVALIDOS
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1716'         TO GFCTG2-COD-MSG
                   MOVE 1716           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1506'         TO GFCTG2-COD-MSG
                   MOVE 1506           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG     TO CMTFWY6I-MENSAGEM
               MOVE SPACES              TO CMTFWY6I-SQLCA
               MOVE 12                  TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA OBTER A DATA DE PROCESSAMENTO.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-OBTER-DATA-PROCESSAMENTO   SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
             SELECT
                     DPROCM_ATUAL
               INTO
                    :GFCTB0A1.DPROCM-ATUAL
               FROM
                     DB2PRD.PARM_DATA_PROCM
              WHERE
                     CSIST_PRINC        = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0A1
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1717'         TO GFCTG2-COD-MSG
                   MOVE 1717           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1507'         TO GFCTG2-COD-MSG
                   MOVE 1507           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA)
                                       TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           MOVE DPROCM-ATUAL OF GFCTB0A1(1:2) TO WRK-DIA-PROC-A1.
           MOVE DPROCM-ATUAL OF GFCTB0A1(4:2) TO WRK-MES-PROC-A1.
           MOVE DPROCM-ATUAL OF GFCTB0A1(7:4) TO WRK-ANO-PROC-A1.


      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA VALIDAR O PACOTE                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTE-PACOTE            SECTION.
      *----------------------------------------------------------------*

           MOVE CMTFWY6I-E-CD-CESTA-PERM TO CSERVC-TARIF    OF GFCTB0C6.
           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO DFIM-PRMSS-PCOTE  OF GFCTB0C6.

           MOVE 'I'                    TO WRK-CINDCD-ADSAO-INDVD.
           MOVE 'A'                    TO WRK-CINDCD-ADSAO-AMBAS.

           EXEC SQL
             SELECT
                     CSERVC_TARIF
               INTO
                    :GFCTB0C6.CSERVC-TARIF
               FROM
                     DB2PRD.PRMSS_ADSAO_PCOTE
              WHERE
                     CSERVC_TARIF       = :GFCTB0C6.CSERVC-TARIF
                AND  DFIM_PRMSS_PCOTE  >  :GFCTB0C6.DFIM-PRMSS-PCOTE
                AND (CINDCD_ADSAO_INDVD = :WRK-CINDCD-ADSAO-INDVD OR
                     CINDCD_ADSAO_INDVD = :WRK-CINDCD-ADSAO-AMBAS)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -811) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0C6
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1739'         TO GFCTG2-COD-MSG
                   MOVE 1739           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1530'         TO GFCTG2-COD-MSG
                   MOVE 1530           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 08                 TO CMTFWY6I-COD-RETORNO
      *
      *--- REGISTRO NAO ENCONTRADO NA TABELA GFCTB0C6
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1740'         TO GFCTG2-COD-MSG
                   MOVE 1740           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1531'         TO GFCTG2-COD-MSG
                   MOVE 1531           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 08                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           PERFORM 1310-ACESSAR-GFCTB0A2.

           IF  WRK-CESTA-CELULAR       EQUAL 'N'
               MOVE 08                 TO CMTFWY6I-COD-RETORNO
      *
      *--- TARIFA PACOTE NAO EH CESTA CELULAR
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1795'         TO GFCTG2-COD-MSG
                   MOVE 1795           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1545'         TO GFCTG2-COD-MSG
                   MOVE 1545           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG     TO CMTFWY6I-MENSAGEM
               MOVE SPACES              TO CMTFWY6I-SQLCA
               MOVE 04                  TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA ACESSAR GFCTB0A2                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1310-ACESSAR-GFCTB0A2           SECTION.
      *----------------------------------------------------------------*

           MOVE CMTFWY6I-E-CD-CESTA-PERM TO CSERVC-TARIF OF GFCTB0A2.

           MOVE DPROCM-ATUAL   OF GFCTB0A1
                               TO DINIC-VGCIA-TARIF   OF GFCTB0A2.

           MOVE DPROCM-ATUAL   OF GFCTB0A1
                               TO DFIM-VGCIA-TARIF    OF GFCTB0A2.

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

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0A2
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1722'         TO GFCTG2-COD-MSG
                   MOVE 1722           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1530'         TO GFCTG2-COD-MSG
                   MOVE 1530           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  CINDCD-CREDT-CLULR OF GFCTB0A2   EQUAL   'S'
               MOVE 'S'                TO WRK-CESTA-CELULAR
           ELSE
               MOVE 'N'                TO WRK-CESTA-CELULAR
           END-IF.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA VALIDAR A CONTA                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1400-VALIDAR-CONTA              SECTION.
      *----------------------------------------------------------------*

           MOVE CMTFWY6I-E-CD-COML-AGENCIA TO LKW-CJUNC-DEPC.
           MOVE CMTFWY6I-E-CD-COML-CONTA   TO LKW-CTA-CORR.

           MOVE 'SCCE9100'             TO WRK-MODULO.

           CALL WRK-MODULO             USING LKW-AREA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- MENSAGEM INTERNA NO PROGRAMA
      *
               MOVE 1392               TO CMTFWY6I-COD-MENSAGEM

               MOVE WRK-MSG-SCCE9100   TO CMTFWY6I-MENSAGEM

               MOVE SPACES             TO CMTFWY6I-SQLCA

               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  LKW-CD-RETORNO          EQUAL 99
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- MENSAGEM INTERNA NO PROGRAMA
      *
               MOVE 1392               TO CMTFWY6I-COD-MENSAGEM

               MOVE WRK-MSG-SCCE9100   TO CMTFWY6I-MENSAGEM

               MOVE SPACES             TO CMTFWY6I-SQLCA

               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  LKW-CD-RETORNO          NOT EQUAL ZEROS
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- PROBLEMA NO ACESSO A BASE DE MENSAGENS
      *
               MOVE 1306               TO CMTFWY6I-COD-MENSAGEM

               MOVE LKW-MENSAGEM       TO CMTFWY6I-MENSAGEM

               MOVE SPACES             TO CMTFWY6I-SQLCA

               MOVE 04                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA VALIDAR AGENCIA/CONTA                              *
      ******************************************************************
      *----------------------------------------------------------------*
       1500-CONSISTE-AGENCIA-CONTA     SECTION.
      *----------------------------------------------------------------*

           MOVE CMTFWY6I-E-CD-COML-AGENCIA TO CJUNC-DEPDC OF CLIEV007.
           MOVE CMTFWY6I-E-CD-COML-CONTA   TO CCTA-CLI    OF CLIEV007.

           EXEC SQL
             SELECT
                     CID_CLI
               INTO
                    :CLIEV007.CID-CLI
               FROM
                     DB2PRD.V01CTA_POUPANCA
              WHERE
                     CJUNC_DEPDC        = :CLIEV007.CJUNC-DEPDC
                AND  CCTA_CLI           = :CLIEV007.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO NA TABELA CLIEV007
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1718'         TO GFCTG2-COD-MSG
                   MOVE 1718           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1508'         TO GFCTG2-COD-MSG
                   MOVE 1508           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 1510-ACESSAR-CLIEV008
           ELSE
               MOVE 2                  TO WRK-TIPO-CONTA-CLIENTE
               MOVE CID-CLI OF CLIEV007
                                       TO CID-CLI OF CLIEV004

               PERFORM 1520-ACESSAR-CLIEV004
           END-IF.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSAR CLIEV008                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       1510-ACESSAR-CLIEV008           SECTION.
      *----------------------------------------------------------------*

           MOVE CMTFWY6I-E-CD-COML-AGENCIA TO CJUNC-DEPDC OF CLIEV008.
           MOVE CMTFWY6I-E-CD-COML-CONTA   TO CCTA-CLI    OF CLIEV008.

           EXEC SQL
             SELECT
                     CID_CLI
               INTO
                    :CLIEV008.CID-CLI
               FROM
                     DB2PRD.V01CTA_CORRENTE
              WHERE
                     CJUNC_DEPDC        = :CLIEV008.CJUNC-DEPDC
                AND  CCTA_CLI           = :CLIEV008.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO NA TABELA CLIEV008
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1719'         TO GFCTG2-COD-MSG
                   MOVE 1719           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1509'         TO GFCTG2-COD-MSG
                   MOVE 1509           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 08                 TO CMTFWY6I-COD-RETORNO
      *
      *--- REGISTRO NAO ENCONTRADO NA TABELA CLIEV008
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1724'         TO GFCTG2-COD-MSG
                   MOVE 1724           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1514'         TO GFCTG2-COD-MSG
                   MOVE 1514           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 04                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           ELSE
               MOVE 1                  TO WRK-TIPO-CONTA-CLIENTE
               MOVE CID-CLI OF CLIEV008
                                       TO CID-CLI OF CLIEV004

               PERFORM 1520-ACESSAR-CLIEV004
           END-IF.

      *----------------------------------------------------------------*
       1510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSAR CLIEV004 PARA OBTER DADOS DO CLIENTE                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1520-ACESSAR-CLIEV004           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                     CCGC_CPF,
                     CFLIAL_CGC,
                     CCTRL_CPF_CGC,
ST25X6               CCGC_CPF_ST,
ST25X6               CFLIAL_CGC_ST,
ST25X6               CCTRL_CPF_CGC_ST,
                     CTPO_PSSOA,
                     IPSSOA_COPLT
               INTO
                    :CLIEV004.CCGC-CPF,
                    :CLIEV004.CFLIAL-CGC,
                    :CLIEV004.CCTRL-CPF-CGC,
ST25X6              :CLIEV004.CCGC-CPF-ST,
ST25X6              :CLIEV004.CFLIAL-CGC-ST,
ST25X6              :CLIEV004.CCTRL-CPF-CGC-ST,
                    :CLIEV004.CTPO-PSSOA,
                    :CLIEV004.IPSSOA-COPLT
               FROM
                     DB2PRD.V01CLIENTE_AGENCIA
              WHERE
                     CID_CLI            = :CLIEV004.CID-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO NA TABELA CLIEV004
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1720'         TO GFCTG2-COD-MSG
                   MOVE 1720           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1510'         TO GFCTG2-COD-MSG
                   MOVE 1510           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- REGISTRO NAO ENCONTRADO NA TABELA CLIEV004
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1725'         TO GFCTG2-COD-MSG
                   MOVE 1725           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1515'         TO GFCTG2-COD-MSG
                   MOVE 1515           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 04                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           ELSE
ST25X6*        1F  CFLIAL-CGC OF CLIEV004
ST25X6*                                EQUAL ZEROS
ST25X6         IF  CFLIAL-CGC-ST OF CLIEV004
ST25X6                                 EQUAL ZEROS
ST25X6         OR  CFLIAL-CGC-ST OF CLIEV004
ST25X6                                 EQUAL SPACES
ST25X6         OR  CFLIAL-CGC-ST OF CLIEV004
ST25X6                                 EQUAL LOW-VALUES
                   MOVE 'F'            TO WRK-TIPO-PESSOA-CLIENTE
               ELSE
                   MOVE 'J'            TO WRK-TIPO-PESSOA-CLIENTE
               END-IF

               PERFORM 1521-OBTER-DADOS-GFCTB0D8
           END-IF.

      *----------------------------------------------------------------*
       1520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER DADOS DA TARIFA.                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1521-OBTER-DADOS-GFCTB0D8       SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0C6
                                       TO CSERVC-TARIF OF GFCTB0D8.

           EXEC SQL
             SELECT
                     CPSSOA_SERVC_TARIF,
                     CTPO_CTA_DEB_TARIF,
                     CTPO_SERVC_TARIF
               INTO
                    :GFCTB0D8.CPSSOA-SERVC-TARIF,
                    :GFCTB0D8.CTPO-CTA-DEB-TARIF,
                    :GFCTB0D8.CTPO-SERVC-TARIF
               FROM
                     DB2PRD.SERVC_TARIF_PRINC
              WHERE
                     CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0D8
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1721'         TO GFCTG2-COD-MSG
                   MOVE 1721           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1511'         TO GFCTG2-COD-MSG
                   MOVE 1511           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF CTPO-SERVC-TARIF OF GFCTB0D8 EQUAL 2
              NEXT SENTENCE
           ELSE
              IF CTPO-SERVC-TARIF OF GFCTB0D8 EQUAL 4
                 MOVE '1926'         TO GFCTG2-COD-MSG
                 MOVE 1926           TO CMTFWY6I-COD-MENSAGEM
                 PERFORM 9900-OBTER-DESC-MSG
                 MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
                 MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA)
                                         TO CMTFWY6I-SQLCA
                 MOVE 12                 TO RETURN-CODE
                 PERFORM 9999-FINALIZAR
              ELSE
                 MOVE '1925'         TO GFCTG2-COD-MSG
                 MOVE 1925           TO CMTFWY6I-COD-MENSAGEM
                 PERFORM 9900-OBTER-DESC-MSG
                 MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
                 MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA)
                                         TO CMTFWY6I-SQLCA
                 MOVE 12                 TO RETURN-CODE
                 PERFORM 9999-FINALIZAR
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       1521-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VALIDAR CODIGO DA OPERADORA                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1600-VALIDAR-OPERADORA          SECTION.
      *----------------------------------------------------------------*

           MOVE CMTFWY6I-E-OPERADORA   TO COPER-FONE-MOVEL OF GFCTB0M4.
           MOVE CMTFWY6I-E-DDD         TO WRK-NUM04-A
           MOVE WRK-NUM03-N            TO CDDD-FONE-MOVEL  OF GFCTB0M4.

           EXEC SQL
             SELECT
                     ROPER_FONE_MOVEL
               INTO
                    :GFCTB0M4.ROPER-FONE-MOVEL
               FROM
                     DB2PRD.TOPER_FONE_CREDT
              WHERE
                     COPER_FONE_MOVEL   = :GFCTB0M4.COPER-FONE-MOVEL
                AND  CDDD_FONE_MOVEL    = :GFCTB0M4.CDDD-FONE-MOVEL
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0M4
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1797'         TO GFCTG2-COD-MSG
                   MOVE 1797           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1547'         TO GFCTG2-COD-MSG
                   MOVE 1547           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 08                 TO CMTFWY6I-COD-RETORNO
      *
      *--- DDD NAO CONDIZENTE COM O CODIGO DA OPERADORA
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1798'         TO GFCTG2-COD-MSG
                   MOVE 1798           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1548'         TO GFCTG2-COD-MSG
                   MOVE 1548           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 04                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VALIDAR NUMERO DA CELULAR                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1700-VALIDAR-CELULAR            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                     WAN7-ENTRADA
                                          WAN7-SAIDA
                                          WAN7-DADOS-ERRO.

           MOVE 'O'                    TO WAN7-AMBIENTE.
           MOVE CMTFWY6I-E-DDD         TO WRK-NUM04-A.
           MOVE WRK-NUM03-N            TO WAN7-CD-DDD.
           MOVE CMTFWY6I-E-NUMERO      TO WAN7-NR-TELEFONE.

           MOVE 'GFCT5307'         TO     WRK-MODULO.

           CALL WRK-MODULO         USING  GFCTWAN7-REG.

           IF WAN7-COD-RETORNO  NOT EQUAL  ZEROS
               PERFORM 9950-OBTEM-MSG-GFCT5307
               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR DADOS OBTIDOS NO CURSOR 1 DA TABELA GFCTB0C6.        *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR-REGISTROS        SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-TARIFA-VALIDA.

           IF  (CTPO-CTA-DEB-TARIF OF GFCTB0D8  EQUAL  3)
           AND (WRK-TIPO-CONTA-CLIENTE          EQUAL  1)
               MOVE   CTPO-CTA-DEB-TARIF OF GFCTB0D8   TO
                      WRK-TIPO-CONTA-CLIENTE
           END-IF.

           IF  CTPO-CTA-DEB-TARIF OF GFCTB0D8
                                       EQUAL WRK-TIPO-CONTA-CLIENTE
               IF  CPSSOA-SERVC-TARIF OF GFCTB0D8
                                       EQUAL '3' OR
                  (CPSSOA-SERVC-TARIF OF GFCTB0D8
                                       EQUAL '1' AND
                   WRK-TIPO-PESSOA-CLIENTE
                                       EQUAL 'F') OR
                  (CPSSOA-SERVC-TARIF OF GFCTB0D8
                                       EQUAL '2' AND
                   WRK-TIPO-PESSOA-CLIENTE
                                       EQUAL 'J')
                   PERFORM 2700-CALCULAR-DATA-FIM

                   MOVE 014            TO WRK-PROX-AGRUPAMENTO

                   MOVE 'N'            TO WRK-FIM-AGRUPAMENTO

                   MOVE 'N'            TO WRK-AGRUPAMENTO-VALIDO

RJOVA *---   DESVIO PARA NAO CHECAR A PERMISSAO NO GFCT SERA
RJOVA *---   A DO GOTF VALIDA

RJOVA            IF CMTFWY6I-E-PERMIS-GOTF
                                       EQUAL 'S'
                   MOVE 'S'            TO WRK-AGRUPAMENTO-VALIDO
RJOVA            END-IF

                   PERFORM 2100-IDENTIFICAR-PERMISSAO
                     UNTIL WRK-FIM-AGRUPAMENTO
                                       EQUAL 'S'
                        OR WRK-AGRUPAMENTO-VALIDO
                                       EQUAL 'S'

                   IF  WRK-AGRUPAMENTO-VALIDO         EQUAL 'S'
                       MOVE 'S'        TO WRK-TARIFA-VALIDA
                   END-IF
               END-IF
           END-IF.

           IF  WRK-TARIFA-VALIDA       EQUAL 'S'
               PERFORM 2400-VER-TARIFA-PACOTE-CAD

               PERFORM 2500-VER-TARIFA-COMP-CAD

               PERFORM 2800-INSERIR-GFCTB009

               IF  CMTFWY6I-COD-RETORNO EQUAL ZEROS
                   PERFORM 3000-INSERIR-GFCTB0M2

                   IF  CMTFWY6I-COD-RETORNO
                                       EQUAL ZEROS
                       MOVE 00         TO CMTFWY6I-COD-RETORNO
      *
      *--- ADESAO AO PACOTE - TARIFA PACOTE DELULAR ADERIDA COM SUCESSO
      *
                       IF  CMTFWY6I-E-AMBIENTE
                                       EQUAL 'O'
                           MOVE '1749' TO GFCTG2-COD-MSG
                           MOVE 1749   TO CMTFWY6I-COD-MENSAGEM
                       ELSE
                           MOVE '1529' TO GFCTG2-COD-MSG
                           MOVE 1529   TO CMTFWY6I-COD-MENSAGEM
                       END-IF

                       PERFORM 9800-OBTER-DESC-MSG-S-FIN

                       IF  CMTFWY6I-COD-RETORNO EQUAL 00
                           MOVE GFCTG3-DESC-MSG
                                       TO CMTFWY6I-MENSAGEM
                           MOVE SPACES TO CMTFWY6I-SQLCA

                           MOVE ZEROS  TO RETURN-CODE

                       ELSE
                           PERFORM 2950-EXCLUIR-GFCTB0M2
                           PERFORM 2900-EXCLUIR-GFCTB009
                       END-IF
                   ELSE
                       PERFORM 2900-EXCLUIR-GFCTB009
                   END-IF
               END-IF
           ELSE
               MOVE 08                TO CMTFWY6I-COD-RETORNO
      *
      *--- ADESAO AO PACOTE - PACOTE NAO EH VALIDO PARA ADESAO
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1743'         TO GFCTG2-COD-MSG
                   MOVE 1743           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1534'         TO GFCTG2-COD-MSG
                   MOVE 1534           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * IDENTIFICAR AGRUPAMENTO COM PERMISSAO DE ADESAO AO PACOTE.     *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-IDENTIFICAR-PERMISSAO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-EXISTE-AGRUPAMENTO.

           EVALUATE WRK-PROX-AGRUPAMENTO

               WHEN 004

                    IF  WRK-5527-CCGC-CPF
                                       NOT EQUAL ZEROS
ST25X6              AND  WRK-5527-CCGC-CPF
ST25X6                                 NOT EQUAL SPACES
ST25X6              AND  WRK-5527-CCGC-CPF
ST25X6                                 NOT EQUAL LOW-VALUES
                        PERFORM 2110-ACESSAR-GFCTB0I0

                        IF  WRK-EXISTE-AGRUPAMENTO
                                       EQUAL 'S'
                            PERFORM 2300-OBTER-DADOS-GFCTB0C6
                        END-IF
                    END-IF

                    IF  WRK-AGRUPAMENTO-VALIDO
                                       EQUAL 'N'
                        MOVE 018       TO WRK-PROX-AGRUPAMENTO
                    END-IF

               WHEN 009

                    IF  WRK-1-VEZ-SEGMENTO
                                       EQUAL  'N'
                        PERFORM 2120-OBTER-SEG-GESTAO

                        MOVE 'S'       TO  WRK-1-VEZ-SEGMENTO
                    END-IF

                    IF  GFCTJM-CSGMTO-GSTAO-TARIF
                                       NOT EQUAL ZEROS
                        PERFORM 2130-ACESSAR-GFCTB0D2

                        IF  WRK-EXISTE-AGRUPAMENTO
                                       EQUAL 'S'
                            PERFORM 2300-OBTER-DADOS-GFCTB0C6
                        END-IF
                    END-IF

                    IF  WRK-AGRUPAMENTO-VALIDO
                                       EQUAL 'N'
                        MOVE 'S'       TO WRK-FIM-AGRUPAMENTO
                    END-IF

               WHEN 014

                    PERFORM 2140-ACESSAR-GFCTB0C7

                    IF  WRK-EXISTE-AGRUPAMENTO
                                       EQUAL 'S'
                        PERFORM 2300-OBTER-DADOS-GFCTB0C6
                    END-IF

                    IF  WRK-AGRUPAMENTO-VALIDO
                                       EQUAL 'N'
                        MOVE 015       TO WRK-PROX-AGRUPAMENTO
                    END-IF

               WHEN 015

                    IF  WRK-1-VEZ-CNPJ-CPF
                                       EQUAL 'N'
                        PERFORM 2150-OBTER-CNPJ-CPF-POSTO

                        MOVE 'S'       TO WRK-1-VEZ-CNPJ-CPF
                    END-IF

                    IF  WRK-5527-CPOSTO-SERVC
                                       NOT EQUAL ZEROS
                        PERFORM 2160-ACESSAR-GFCTB0D0

                        IF  WRK-EXISTE-AGRUPAMENTO
                                       EQUAL 'S'
                            PERFORM 2300-OBTER-DADOS-GFCTB0C6
                        END-IF
                    END-IF

                    IF  WRK-AGRUPAMENTO-VALIDO
                                       EQUAL 'N'
                        MOVE 017       TO WRK-PROX-AGRUPAMENTO
                    END-IF

               WHEN 017

                    IF  WRK-5527-CPOSTO-SERVC
                                       NOT EQUAL ZEROS
                        PERFORM 2170-ACESSAR-GFCTB0D1

                        IF  WRK-EXISTE-AGRUPAMENTO
                                       EQUAL 'S'
                            PERFORM 2300-OBTER-DADOS-GFCTB0C6
                        END-IF
                    END-IF

                    IF  WRK-AGRUPAMENTO-VALIDO
                                       EQUAL 'N'
                        MOVE 020       TO WRK-PROX-AGRUPAMENTO
                    END-IF

               WHEN 018

                    IF  WRK-1-VEZ-UF-MUNIC
                                       EQUAL  'N'
                        PERFORM 2180-OBTER-UF-MUNICIPIO

                        MOVE 'S'       TO WRK-1-VEZ-UF-MUNIC
                    END-IF

                    IF  GFCTRK-CMUN-RURAL
                                       NOT EQUAL ZEROS
                        PERFORM 2190-ACESSAR-GFCTB0C9

                        IF  WRK-EXISTE-AGRUPAMENTO
                                       EQUAL 'S'
                            PERFORM 2300-OBTER-DADOS-GFCTB0C6
                        END-IF
                    END-IF

                    IF  WRK-AGRUPAMENTO-VALIDO
                                       EQUAL 'N'
                        MOVE 019       TO WRK-PROX-AGRUPAMENTO
                    END-IF

               WHEN 019

                    IF  GFCTRK-CSGL-UF NOT EQUAL SPACES
                        PERFORM 2200-ACESSAR-GFCTB0D3

                        IF  WRK-EXISTE-AGRUPAMENTO
                                       EQUAL 'S'
                            PERFORM 2300-OBTER-DADOS-GFCTB0C6
                        END-IF
                    END-IF

                    IF  WRK-AGRUPAMENTO-VALIDO
                                       EQUAL 'N'
                        MOVE 009       TO WRK-PROX-AGRUPAMENTO
                    END-IF

               WHEN 020

                    IF  WRK-5527-CPOSTO-SERVC
                                       NOT EQUAL ZEROS
                        PERFORM 2170-ACESSAR-GFCTB0D1

                        IF  WRK-EXISTE-AGRUPAMENTO
                                       EQUAL 'S'
                            PERFORM 2300-OBTER-DADOS-GFCTB0C6
                        END-IF
                    END-IF

                    IF  WRK-AGRUPAMENTO-VALIDO
                                       EQUAL 'N'
                        MOVE 004       TO WRK-PROX-AGRUPAMENTO
                    END-IF

           END-EVALUATE.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO CLIENTE.*
      ******************************************************************
      *----------------------------------------------------------------*
       2110-ACESSAR-GFCTB0I0           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0I0.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0I0.
           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0I0.
ST25X6*    MOVE WRK-5527-CCGC-CPF      TO CCGC-CPF          OF GFCTB0I0.
ST25X6*    MOVE WRK-5527-CFLIAL-CGC    TO CFLIAL-CGC        OF GFCTB0I0.
ST25X6*    MOVE WRK-5527-CCTRL-CPF-CGC TO CCTRL-CPF-CGC     OF GFCTB0I0.
ST25X6     MOVE WRK-5527-CCGC-CPF      TO CCGC-CPF-ST       OF GFCTB0I0.
ST25X6     MOVE WRK-5527-CFLIAL-CGC    TO CFLIAL-CGC-ST     OF GFCTB0I0.
ST25X6     MOVE WRK-5527-CCTRL-CPF-CGC TO CCTRL-CPF-CGC-ST  OF GFCTB0I0.

           PERFORM 2111-OBTER-MAX-DATA-INICIO.

           EXEC SQL
             SELECT
                     CSEQ_AGPTO_CTA,
                     DINIC_PRMSS_PCOTE
               INTO
                    :GFCTB0I0.CSEQ-AGPTO-CTA,
                    :GFCTB0I0.DINIC-PRMSS-PCOTE
               FROM
                     DB2PRD.TPRMSS_GRP_CLI
              WHERE
                     CSERVC_TARIF       = :GFCTB0I0.CSERVC-TARIF
                AND  CAGPTO_CTA         = :GFCTB0I0.CAGPTO-CTA
ST25X6*         AND  CCGC_CPF           = :GFCTB0I0.CCGC-CPF
ST25X6*         AND  CFLIAL_CGC         = :GFCTB0I0.CFLIAL-CGC
ST25X6*         AND  CCTRL_CPF_CGC      = :GFCTB0I0.CCTRL-CPF-CGC
ST25X6          AND  CCGC_CPF_ST        = :GFCTB0I0.CCGC-CPF-ST
ST25X6          AND  CFLIAL_CGC_ST      = :GFCTB0I0.CFLIAL-CGC-ST
ST25X6          AND  CCTRL_CPF_CGC_ST   = :GFCTB0I0.CCTRL-CPF-CGC-ST
                AND  DINIC_PRMSS_PCOTE  = :GFCTB0I0.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0I0
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1726'         TO GFCTG2-COD-MSG
                   MOVE 1726           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1516'         TO GFCTG2-COD-MSG
                   MOVE 1516           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2600-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                             WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA OF GFCTB0I0
                                       TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0I0
                                       TO WRK-DINIC-PRMSS-PCOTE

                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER A MAIOR DATA DE INICIO DA PERMISSAO                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2111-OBTER-MAX-DATA-INICIO      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                     MAX(DINIC_PRMSS_PCOTE)
               INTO
                    :GFCTB0I0.DINIC-PRMSS-PCOTE
               FROM
                     DB2PRD.TPRMSS_GRP_CLI
              WHERE
                     CSERVC_TARIF       = :GFCTB0I0.CSERVC-TARIF
                AND  CAGPTO_CTA         = :GFCTB0I0.CAGPTO-CTA
ST25X6*         AND  CCGC_CPF           = :GFCTB0I0.CCGC-CPF
ST25X6*         AND  CFLIAL_CGC         = :GFCTB0I0.CFLIAL-CGC
ST25X6*         AND  CCTRL_CPF_CGC      = :GFCTB0I0.CCTRL-CPF-CGC
ST25X6          AND  CCGC_CPF_ST        = :GFCTB0I0.CCGC-CPF-ST
ST25X6          AND  CFLIAL_CGC_ST      = :GFCTB0I0.CFLIAL-CGC-ST
ST25X6          AND  CCTRL_CPF_CGC_ST   = :GFCTB0I0.CCTRL-CPF-CGC-ST
                AND  DINIC_PRMSS_PCOTE <= :GFCTB0I0.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0I0
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1726'         TO GFCTG2-COD-MSG
                   MOVE 1726           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1516'         TO GFCTG2-COD-MSG
                   MOVE 1516           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -305
                MOVE '01.01.0001'      TO DINIC-PRMSS-PCOTE OF GFCTB0I0
           END-IF.

      *----------------------------------------------------------------*
       2111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSAR MODULO GFCT5534 PARA OBTER SEGMENTO GESTAO.            *
      ******************************************************************
      *----------------------------------------------------------------*
       2120-OBTER-SEG-GESTAO           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTJM-AREA.

           INITIALIZE                  GFCTJM-AREA.

           MOVE CMTFWY6I-E-AMBIENTE        TO GFCTJM-TPO-AMBIENTE.
           MOVE CMTFWY6I-E-CD-COML-AGENCIA TO GFCTJM-CJUNC-DEPDC.
           MOVE CMTFWY6I-E-CD-COML-CONTA   TO GFCTJM-CCTA-CLI.

           MOVE 'GFCT5534'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTJM-AREA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO AO ACESSAR O MODULO GFCT5534
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1728'         TO GFCTG2-COD-MSG
                   MOVE 1728           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1518'         TO GFCTG2-COD-MSG
                   MOVE 1518           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SPACES             TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTJM-COD-RETORNO      EQUAL 99
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- REGISTRO NAO ENCONTRADO GFCT5534
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1729'         TO GFCTG2-COD-MSG
                   MOVE 1729           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1519'         TO GFCTG2-COD-MSG
                   MOVE 1519           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SPACES             TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTJM-COD-RETORNO      NOT EQUAL ZEROS
               MOVE ZEROS              TO GFCTJM-CSGMTO-GSTAO-TARIF
           END-IF.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO SEGMENTO*
      ******************************************************************
      *----------------------------------------------------------------*
       2130-ACESSAR-GFCTB0D2           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D2.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0D2.
           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0D2.
           MOVE GFCTJM-CSGMTO-GSTAO-TARIF
                                       TO CSGMTO-GSTAO-TARIF
                                                            OF GFCTB0D2.

           PERFORM 2131-OBTER-MAX-DATA-INICIO.

           EXEC SQL
             SELECT
                     CSEQ_AGPTO_CTA,
                     DINIC_PRMSS_PCOTE
               INTO
                    :GFCTB0D2.CSEQ-AGPTO-CTA,
                    :GFCTB0D2.DINIC-PRMSS-PCOTE
               FROM
                     DB2PRD.PRMSS_GRP_SGMTO
              WHERE
                     CSERVC_TARIF       = :GFCTB0D2.CSERVC-TARIF
                AND  CAGPTO_CTA         = :GFCTB0D2.CAGPTO-CTA
                AND  CSGMTO_GSTAO_TARIF = :GFCTB0D2.CSGMTO-GSTAO-TARIF
                AND  DINIC_PRMSS_PCOTE  = :GFCTB0D2.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0D2
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1730'         TO GFCTG2-COD-MSG
                   MOVE 1730           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1520'         TO GFCTG2-COD-MSG
                   MOVE 1520           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2600-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                             WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA OF GFCTB0D2
                                       TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0D2
                                       TO WRK-DINIC-PRMSS-PCOTE

                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER A MAIOR DATA DE INICIO DA PERMISSAO                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2131-OBTER-MAX-DATA-INICIO      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                     MAX(DINIC_PRMSS_PCOTE)
               INTO
                    :GFCTB0D2.DINIC-PRMSS-PCOTE
               FROM
                     DB2PRD.PRMSS_GRP_SGMTO
              WHERE
                     CSERVC_TARIF       = :GFCTB0D2.CSERVC-TARIF
                AND  CAGPTO_CTA         = :GFCTB0D2.CAGPTO-CTA
                AND  CSGMTO_GSTAO_TARIF = :GFCTB0D2.CSGMTO-GSTAO-TARIF
                AND  DINIC_PRMSS_PCOTE <= :GFCTB0D2.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0D2
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1730'         TO GFCTG2-COD-MSG
                   MOVE 1730           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1520'         TO GFCTG2-COD-MSG
                   MOVE 1520           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -305
               MOVE '01.01.0001'       TO DINIC-PRMSS-PCOTE OF GFCTB0D2
           END-IF.

      *----------------------------------------------------------------*
       2131-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO AGENCIA.*
      ******************************************************************
      *----------------------------------------------------------------*
       2140-ACESSAR-GFCTB0C7           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0C7.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0C7.
           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C7.
           MOVE CMTFWY6I-E-CD-COML-AGENCIA TO CDEPDC        OF GFCTB0C7.

           PERFORM 2141-OBTER-MAX-DATA-INICIO.

           EXEC SQL
             SELECT
                     CSEQ_AGPTO_CTA,
                     DINIC_PRMSS_PCOTE
               INTO
                    :GFCTB0C7.CSEQ-AGPTO-CTA,
                    :GFCTB0C7.DINIC-PRMSS-PCOTE
               FROM
                     DB2PRD.PRMSS_GRP_DEPDC
              WHERE
                     CSERVC_TARIF       = :GFCTB0C7.CSERVC-TARIF
                AND  CAGPTO_CTA         = :GFCTB0C7.CAGPTO-CTA
                AND (CDEPDC             = 0
                 OR  CDEPDC             = :GFCTB0C7.CDEPDC)
                AND  DINIC_PRMSS_PCOTE  = :GFCTB0C7.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0C7
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1731'         TO GFCTG2-COD-MSG
                   MOVE 1731           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1521'         TO GFCTG2-COD-MSG
                   MOVE 1521           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2600-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                             WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA OF GFCTB0C7
                                       TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0C7
                                       TO WRK-DINIC-PRMSS-PCOTE

                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER A MAIOR DATA DE INICIO DA PERMISSAO                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2141-OBTER-MAX-DATA-INICIO      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                     MAX(DINIC_PRMSS_PCOTE)
               INTO
                    :GFCTB0C7.DINIC-PRMSS-PCOTE
               FROM
                     DB2PRD.PRMSS_GRP_DEPDC
              WHERE
                     CSERVC_TARIF       = :GFCTB0C7.CSERVC-TARIF
                AND  CAGPTO_CTA         = :GFCTB0C7.CAGPTO-CTA
                AND (CDEPDC             = 0
                 OR  CDEPDC             = :GFCTB0C7.CDEPDC)
                AND  DINIC_PRMSS_PCOTE <= :GFCTB0C7.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0C7
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1731'         TO GFCTG2-COD-MSG
                   MOVE 1731           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1521'         TO GFCTG2-COD-MSG
                   MOVE 1521           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -305
               MOVE '01.01.0001'       TO DINIC-PRMSS-PCOTE OF GFCTB0C7
           END-IF.

      *----------------------------------------------------------------*
       2141-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSAR MODULO GFCT5527 PARA OBTER CPF/CNPJ/POSTO.             *
      ******************************************************************
      *----------------------------------------------------------------*
       2150-OBTER-CNPJ-CPF-POSTO       SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-5527-AREA-SAIDA.

           INITIALIZE                  WRK-5527-AREA-SAIDA.

           MOVE 'GFCT6046'                 TO WRK-5527-CHAMADOR.
           MOVE CMTFWY6I-E-CD-COML-AGENCIA TO WRK-5527-AGENCIA-CLIENTE.
           MOVE CMTFWY6I-E-CD-COML-CONTA   TO WRK-5527-CONTA-CLIENTE.
           MOVE CMTFWY6I-E-AMBIENTE        TO WRK-5527-AMBIENTE.

           MOVE 'GFCT5527'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-5527-AREA-ENTRADA
                                             WRK-5527-AREA-SAIDA
                                             WRK-IO-PCB
                                             WRK-ALT-PCB
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ADESAO AO PACOTE - ERRO AO ACESSAR O MODULO GFCT5527
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1723'         TO GFCTG2-COD-MSG
                   MOVE 1723           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1513'         TO GFCTG2-COD-MSG
                   MOVE 1513           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SPACES             TO CMTFWY6I-SQLCA
               MOVE 04                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF (WRK-5527-COD-RETORNO    NOT EQUAL ZEROS AND 02 AND 05)
               IF  WRK-5527-COD-RETORNO
                                       EQUAL 99
                   MOVE 12             TO CMTFWY6I-COD-RETORNO
      *
      *--- ADESAO AO PACOTE - REGISTRO NAO ENCONTRADO GFCT5527
      *
                   IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                       MOVE '1727'     TO GFCTG2-COD-MSG
                       MOVE 1727       TO CMTFWY6I-COD-MENSAGEM
                   ELSE
                       MOVE '1517'     TO GFCTG2-COD-MSG
                       MOVE 1517       TO CMTFWY6I-COD-MENSAGEM
                   END-IF

                   PERFORM 9900-OBTER-DESC-MSG

                   MOVE GFCTG3-DESC-MSG
                                       TO CMTFWY6I-MENSAGEM
                   MOVE SPACES         TO CMTFWY6I-SQLCA
                   MOVE 12             TO RETURN-CODE

                   PERFORM 9999-FINALIZAR
               END-IF
           END-IF.

           IF (WRK-5527-COD-RETORNO    EQUAL 02 OR 05)
ST25X6*         MOVE ZEROS              TO WRK-5527-CCGC-CPF
ST25X6*                                    WRK-5527-CFLIAL-CGC
ST25X6*                                    WRK-5527-CCTRL-CPF-CGC
ST25X6          MOVE SPACES             TO WRK-5527-CCGC-CPF
ST25X6                                    WRK-5527-CFLIAL-CGC
ST25X6          MOVE ZEROS              TO WRK-5527-CCTRL-CPF-CGC

           END-IF.

      *----------------------------------------------------------------*
       2150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO PAB.    *
      ******************************************************************
      *----------------------------------------------------------------*
       2160-ACESSAR-GFCTB0D0           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D0.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0D0.
           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0D0.
           MOVE CMTFWY6I-E-CD-COML-AGENCIA TO CDEPDC        OF GFCTB0D0.

           MOVE WRK-5527-CPOSTO-SERVC  TO CPOSTO-SERVC      OF GFCTB0D0.

           PERFORM 2161-OBTER-MAX-DATA-INICIO.

           EXEC SQL
             SELECT
                     CSEQ_AGPTO_CTA,
                     DINIC_PRMSS_PCOTE
               INTO
                    :GFCTB0D0.CSEQ-AGPTO-CTA,
                    :GFCTB0D0.DINIC-PRMSS-PCOTE
               FROM
                     DB2PRD.PRMSS_GRP_PAB
              WHERE
                     CSERVC_TARIF       = :GFCTB0D0.CSERVC-TARIF
                AND  CAGPTO_CTA         = :GFCTB0D0.CAGPTO-CTA
                AND  CDEPDC             = :GFCTB0D0.CDEPDC
                AND (CPOSTO_SERVC       = 0
                 OR  CPOSTO_SERVC       = :GFCTB0D0.CPOSTO-SERVC)
                AND  DINIC_PRMSS_PCOTE  = :GFCTB0D0.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0D0
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1732'         TO GFCTG2-COD-MSG
                   MOVE 1732           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1522'         TO GFCTG2-COD-MSG
                   MOVE 1522           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2600-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                             WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA OF GFCTB0D0
                                       TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0D0
                                       TO WRK-DINIC-PRMSS-PCOTE

                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2160-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER A MAIOR DATA DE INICIO DA PERMISSAO                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2161-OBTER-MAX-DATA-INICIO      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                     MAX(DINIC_PRMSS_PCOTE)
               INTO
                    :GFCTB0D0.DINIC-PRMSS-PCOTE
               FROM
                     DB2PRD.PRMSS_GRP_PAB
              WHERE
                     CSERVC_TARIF       = :GFCTB0D0.CSERVC-TARIF
                AND  CAGPTO_CTA         = :GFCTB0D0.CAGPTO-CTA
                AND  CDEPDC             = :GFCTB0D0.CDEPDC
                AND (CPOSTO_SERVC       = 0
                 OR  CPOSTO_SERVC       = :GFCTB0D0.CPOSTO-SERVC)
                AND  DINIC_PRMSS_PCOTE <= :GFCTB0D0.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0D0
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1732'         TO GFCTG2-COD-MSG
                   MOVE 1732           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1522'         TO GFCTG2-COD-MSG
                   MOVE 1522           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -305
               MOVE '01.01.0001'       TO DINIC-PRMSS-PCOTE OF GFCTB0D0
           END-IF.

      *----------------------------------------------------------------*
       2161-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO PSTAL.  *
      ******************************************************************
      *----------------------------------------------------------------*
       2170-ACESSAR-GFCTB0D1           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D1.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0D1.
           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0D1.
           MOVE CMTFWY6I-E-CD-COML-AGENCIA TO CDEPDC        OF GFCTB0D1.

           MOVE WRK-5527-CPOSTO-SERVC  TO CPOSTO-SERVC      OF GFCTB0D1.

           PERFORM 2171-OBTER-MAX-DATA-INICIO.

           EXEC SQL
             SELECT
                     CSEQ_AGPTO_CTA,
                     DINIC_PRMSS_PCOTE
               INTO
                    :GFCTB0D1.CSEQ-AGPTO-CTA,
                    :GFCTB0D1.DINIC-PRMSS-PCOTE
               FROM
                     DB2PRD.PRMSS_GRP_PSTAL
              WHERE
                     CSERVC_TARIF       = :GFCTB0D1.CSERVC-TARIF
                AND  CAGPTO_CTA         = :GFCTB0D1.CAGPTO-CTA
                AND (CDEPDC             = 0
                 OR  CDEPDC             = :GFCTB0D1.CDEPDC
                AND  CPOSTO_SERVC       = :GFCTB0D1.CPOSTO-SERVC)
                AND  DINIC_PRMSS_PCOTE  = :GFCTB0D1.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0D1
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1733'         TO GFCTG2-COD-MSG
                   MOVE 1733           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1523'         TO GFCTG2-COD-MSG
                   MOVE 1523           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2600-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                             WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA OF GFCTB0D1
                                       TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0D1
                                       TO WRK-DINIC-PRMSS-PCOTE

                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2170-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER A MAIOR DATA DE INICIO DA PERMISSAO                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2171-OBTER-MAX-DATA-INICIO      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                     MAX(DINIC_PRMSS_PCOTE)
               INTO
                    :GFCTB0D1.DINIC-PRMSS-PCOTE
               FROM
                     DB2PRD.PRMSS_GRP_PSTAL
              WHERE
                     CSERVC_TARIF       = :GFCTB0D1.CSERVC-TARIF
                AND  CAGPTO_CTA         = :GFCTB0D1.CAGPTO-CTA
                AND (CDEPDC             = 0
                 OR  CDEPDC             = :GFCTB0D1.CDEPDC
                AND  CPOSTO_SERVC       = :GFCTB0D1.CPOSTO-SERVC)
                AND  DINIC_PRMSS_PCOTE <= :GFCTB0D1.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0D1
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1733'         TO GFCTG2-COD-MSG
                   MOVE 1733           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1523'         TO GFCTG2-COD-MSG
                   MOVE 1523           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -305
               MOVE '01.01.0001'       TO DINIC-PRMSS-PCOTE OF GFCTB0D1
           END-IF.

      *----------------------------------------------------------------*
       2171-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSAR MODULO GFCT5564 PARA OBTER UF E MUNICIPIO.             *
      ******************************************************************
      *----------------------------------------------------------------*
       2180-OBTER-UF-MUNICIPIO         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTRK-AREA.

           INITIALIZE GFCTRK-AREA.

           MOVE CMTFWY6I-E-AMBIENTE        TO GFCTRK-TPO-AMBIENTE.
           MOVE CMTFWY6I-E-CD-COML-AGENCIA TO GFCTRK-CJUNC-DEPDC.

           MOVE 'GFCT5564'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTRK-AREA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ADESAO AO PACOTE - ERRO AO ACESSAR O MODULO GFCT5564
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1734'         TO GFCTG2-COD-MSG
                   MOVE 1734           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1524'         TO GFCTG2-COD-MSG
                   MOVE 1524           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE 12                 TO RETURN-CODE
               MOVE SPACES             TO CMTFWY6I-SQLCA

               PERFORM 9999-FINALIZAR
           END-IF.

           IF (GFCTRK-COD-RETORNO      EQUAL 01 OR 02 OR 99)
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ADESAO AO PACOTE - REGISTRO NAO ENCONTRADO GFCT5564
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1735'         TO GFCTG2-COD-MSG
                   MOVE 1735           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1525'         TO GFCTG2-COD-MSG
                   MOVE 1525           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SPACES             TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTRK-COD-RETORNO      NOT EQUAL ZEROS
               MOVE ZEROS              TO GFCTRK-CMUN-RURAL
               MOVE SPACES             TO GFCTRK-CSGL-UF
           END-IF.

      *----------------------------------------------------------------*
       2180-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GP MUNICIPIO. *
      ******************************************************************
      *----------------------------------------------------------------*
       2190-ACESSAR-GFCTB0C9           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0C9.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0C9.
           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C9.
           MOVE GFCTRK-CMUN-RURAL      TO WRK-NUM09-N.
           MOVE WRK-NUM07-N            TO CMUN-IBGE         OF GFCTB0C9.

           PERFORM 2191-OBTER-MAX-DATA-INICIO.

           EXEC SQL
             SELECT
                     CSEQ_AGPTO_CTA,
                     DINIC_PRMSS_PCOTE
               INTO
                    :GFCTB0C9.CSEQ-AGPTO-CTA,
                    :GFCTB0C9.DINIC-PRMSS-PCOTE
               FROM
                     DB2PRD.PRMSS_GRP_MUN
              WHERE
                     CSERVC_TARIF       = :GFCTB0C9.CSERVC-TARIF
                AND  CAGPTO_CTA         = :GFCTB0C9.CAGPTO-CTA
                AND  CMUN_IBGE          = :GFCTB0C9.CMUN-IBGE
                AND  DINIC_PRMSS_PCOTE  = :GFCTB0C9.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0C9
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1736'         TO GFCTG2-COD-MSG
                   MOVE 1736           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1526'         TO GFCTG2-COD-MSG
                   MOVE 1526           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2600-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                             WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA OF GFCTB0C9
                                       TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0C9
                                       TO WRK-DINIC-PRMSS-PCOTE

                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2190-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER A MAIOR DATA DE INICIO DA PERMISSAO                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2191-OBTER-MAX-DATA-INICIO      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                     MAX(DINIC_PRMSS_PCOTE)
               INTO
                    :GFCTB0C9.DINIC-PRMSS-PCOTE
               FROM
                     DB2PRD.PRMSS_GRP_MUN
              WHERE
                     CSERVC_TARIF       = :GFCTB0C9.CSERVC-TARIF
                AND  CAGPTO_CTA         = :GFCTB0C9.CAGPTO-CTA
                AND  CMUN_IBGE          = :GFCTB0C9.CMUN-IBGE
                AND  DINIC_PRMSS_PCOTE <= :GFCTB0C9.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0C9
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1736'         TO GFCTG2-COD-MSG
                   MOVE 1736           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1526'         TO GFCTG2-COD-MSG
                   MOVE 1526           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -305
               MOVE '01.01.0001'       TO DINIC-PRMSS-PCOTE OF GFCTB0C9
           END-IF.

      *----------------------------------------------------------------*
       2191-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO UF.     *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-ACESSAR-GFCTB0D3           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D3.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0D3.
           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0D3.
           MOVE GFCTRK-CSGL-UF         TO CSGL-UF           OF GFCTB0D3.

           PERFORM 2201-OBTER-MAX-DATA-INICIO.

           EXEC SQL
             SELECT
                     CSEQ_AGPTO_CTA,
                     DINIC_PRMSS_PCOTE
               INTO
                    :GFCTB0D3.CSEQ-AGPTO-CTA,
                    :GFCTB0D3.DINIC-PRMSS-PCOTE
               FROM
                     DB2PRD.PRMSS_GRP_UF
              WHERE
                     CSERVC_TARIF       = :GFCTB0D3.CSERVC-TARIF
                AND  CAGPTO_CTA         = :GFCTB0D3.CAGPTO-CTA
                AND  CSGL_UF            = :GFCTB0D3.CSGL-UF
                AND  DINIC_PRMSS_PCOTE  = :GFCTB0D3.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0D3
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1737'         TO GFCTG2-COD-MSG
                   MOVE 1737           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1527'         TO GFCTG2-COD-MSG
                   MOVE 1527           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2600-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                             WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA OF GFCTB0D3
                                       TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0D3
                                       TO WRK-DINIC-PRMSS-PCOTE

                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER A MAIOR DATA DE INICIO DA PERMISSAO                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2201-OBTER-MAX-DATA-INICIO      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                     MAX(DINIC_PRMSS_PCOTE)
               INTO
                    :GFCTB0D3.DINIC-PRMSS-PCOTE
               FROM
                     DB2PRD.PRMSS_GRP_UF
              WHERE
                     CSERVC_TARIF       = :GFCTB0D3.CSERVC-TARIF
                AND  CAGPTO_CTA         = :GFCTB0D3.CAGPTO-CTA
                AND  CSGL_UF            = :GFCTB0D3.CSGL-UF
                AND  DINIC_PRMSS_PCOTE <= :GFCTB0D3.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0D3
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1737'         TO GFCTG2-COD-MSG
                   MOVE 1737           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1527'         TO GFCTG2-COD-MSG
                   MOVE 1527           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -305
               MOVE '01.01.0001'       TO DINIC-PRMSS-PCOTE OF GFCTB0D3
           END-IF.

      *----------------------------------------------------------------*
       2201-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER DADOS DA PERMISSAO DE ADESAO AO PACOTE.                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-OBTER-DADOS-GFCTB0C6       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0C6.
           MOVE WRK-SEQ-AGPTO          TO CSEQ-AGPTO-CTA    OF GFCTB0C6.
           MOVE WRK-DINIC-PRMSS-PCOTE  TO DINIC-PRMSS-PCOTE OF GFCTB0C6.

           MOVE 'I'                    TO WRK-CINDCD-ADSAO-INDVD.
           MOVE 'A'                    TO WRK-CINDCD-ADSAO-AMBAS.

           EXEC SQL
             SELECT
                     CSEQ_AGPTO_CTA,
                     DINIC_PRMSS_PCOTE,
                     CINDCD_AGPTO_TOT
               INTO
                    :GFCTB0C6.CSEQ-AGPTO-CTA,
                    :GFCTB0C6.DINIC-PRMSS-PCOTE,
                    :GFCTB0C6.CINDCD-AGPTO-TOT
               FROM
                     DB2PRD.PRMSS_ADSAO_PCOTE
              WHERE
                     CSERVC_TARIF       = :GFCTB0C6.CSERVC-TARIF
                AND  CAGPTO_CTA         = :GFCTB0C6.CAGPTO-CTA
                AND  CSEQ_AGPTO_CTA     = :GFCTB0C6.CSEQ-AGPTO-CTA
                AND  DINIC_PRMSS_PCOTE  = :GFCTB0C6.DINIC-PRMSS-PCOTE
                AND (CINDCD_ADSAO_INDVD = :WRK-CINDCD-ADSAO-INDVD OR
                     CINDCD_ADSAO_INDVD = :WRK-CINDCD-ADSAO-AMBAS)
                AND  CIDTFD_PRMSS_INDVD
                                       IN ( 0, 2 )
                AND  DFIM_PRMSS_PCOTE  > :GFCTB0C6.DFIM-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -811) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0C6
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1738'         TO GFCTG2-COD-MSG
                   MOVE 1738           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1528'         TO GFCTG2-COD-MSG
                   MOVE 1528           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF (SQLCODE                 EQUAL ZEROS OR -811)
               MOVE 'S'                TO WRK-AGRUPAMENTO-VALIDO
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR ADESAO A TARIFA PACORE                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-VER-TARIFA-PACOTE-CAD      SECTION.
      *----------------------------------------------------------------*

           MOVE CMTFWY6I-E-CD-COML-AGENCIA TO CJUNC-DEPDC  OF GFCTB009.
           MOVE CMTFWY6I-E-CD-COML-CONTA   TO CCTA-CLI     OF GFCTB009.

           MOVE '01.01.0001'           TO WRK-DATA-DB2.
           MOVE WRK-DIA-INI-ADSAO-INDVD
                                       TO WRK-DIA-DB2.
           MOVE WRK-MES-INI-ADSAO-INDVD
                                       TO WRK-MES-DB2.
           MOVE WRK-ANO-INI-ADSAO-INDVD
                                       TO WRK-ANO-DB2.
           MOVE WRK-DATA-DB2           TO DFIM-ADSAO-INDVD OF GFCTB009.

RJOVA      IF CMTFWY6I-E-PERMIS-GOTF
                                       EQUAL 'S'
              MOVE '01.01.0001'        TO DFIM-ADSAO-INDVD OF GFCTB009
RJOVA      END-IF.

           MOVE 0                      TO CINDCD-EXCL-REG  OF GFCTB009.

           EXEC SQL
             SELECT
                     DINIC_ADSAO_INDVD,
                     DFIM_ADSAO_INDVD
               INTO
                    :GFCTB009.DINIC-ADSAO-INDVD,
                    :GFCTB009.DFIM-ADSAO-INDVD
               FROM
                     DB2PRD.ADSAO_INDVD_PCOTE
              WHERE
                     CJUNC_DEPDC        = :GFCTB009.CJUNC-DEPDC
                AND  CCTA_CLI           = :GFCTB009.CCTA-CLI
                AND  CINDCD_EXCL_REG    = :GFCTB009.CINDCD-EXCL-REG
                AND  DFIM_ADSAO_INDVD  >  :GFCTB009.DFIM-ADSAO-INDVD
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -811) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ADESAO AO PACOTE - ERRO NA TABELA DB2PRD.ADSAO_INDVD_PCOTE
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1742'         TO GFCTG2-COD-MSG
                   MOVE 1742           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1533'         TO GFCTG2-COD-MSG
                   MOVE 1533           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA

               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF (SQLCODE                 EQUAL ZEROS OR -811)
               MOVE 08                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ADESAO AO PACOTE - REGISTRO JA EXISTE NA TABELA GFCTB009
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1741'         TO GFCTG2-COD-MSG
                   MOVE 1741           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1532'         TO GFCTG2-COD-MSG
                   MOVE 1532           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA

               MOVE 04                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VALIDAR PACOTE.                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-VER-TARIFA-COMP-CAD        SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM-CURSOR
                                          WRK-EXISTE-COMPONENTE.

           PERFORM 2520-ABRIR-CURSOR.

           PERFORM 2530-LER-CURSOR.

           PERFORM 2510-PROCESSAR-CURSOR
             UNTIL WRK-FIM-CURSOR      EQUAL 'S'
                OR WRK-EXISTE-COMPONENTE
                                       EQUAL 'S'.

           PERFORM 2540-FECHAR-CURSOR.

           IF  WRK-EXISTE-COMPONENTE   EQUAL 'S'
               MOVE 08                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ADESAO AO PACOTE - TAR. COMP. JA CONTEMPLADA EM OUTRA TARIFA
      *--- PACOTE
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1745'         TO GFCTG2-COD-MSG
                   MOVE 1745           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1539'         TO GFCTG2-COD-MSG
                   MOVE 1539           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SPACES             TO CMTFWY6I-SQLCA
               MOVE 04                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR CURSOR DA TABELA GFCTB029.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2510-PROCESSAR-CURSOR           SECTION.
      *----------------------------------------------------------------*

           MOVE CMTFWY6I-E-CD-COML-AGENCIA TO CJUNC-DEPDC  OF GFCTB009.
           MOVE CMTFWY6I-E-CD-COML-CONTA   TO CCTA-CLI     OF GFCTB009.
           MOVE ZEROS                  TO CINDCD-EXCL-REG  OF GFCTB009.

           MOVE '01.01.0001'           TO WRK-DATA-DB2.
           MOVE WRK-DIA-INI-ADSAO-INDVD
                                       TO WRK-DIA-DB2.
           MOVE WRK-MES-INI-ADSAO-INDVD
                                       TO WRK-MES-DB2.
           MOVE WRK-ANO-INI-ADSAO-INDVD
                                       TO WRK-ANO-DB2.
           MOVE WRK-DATA-DB2           TO DFIM-ADSAO-INDVD OF GFCTB009.

RJOVA      IF CMTFWY6I-E-PERMIS-GOTF
                                       EQUAL 'S'
              MOVE '01.01.0001'        TO DFIM-ADSAO-INDVD OF GFCTB009
RJOVA      END-IF.

           EXEC SQL
             SELECT
                     DISTINCT (B.CTARIF_PCOTE)
               INTO
                    :GFCTB029.CTARIF-PCOTE
               FROM
                     DB2PRD.ADSAO_INDVD_PCOTE A,
                     DB2PRD.COMPS_TARIF_PCOTE B
              WHERE
                     A.CJUNC_DEPDC      = :GFCTB009.CJUNC-DEPDC
                AND  A.CCTA_CLI         = :GFCTB009.CCTA-CLI
                AND  A.CINDCD_EXCL_REG  = :GFCTB009.CINDCD-EXCL-REG
                AND  A.DFIM_ADSAO_INDVD
                                       >= :GFCTB009.DFIM-ADSAO-INDVD
                AND  A.CSERVC_TARIF     = B.CTARIF_PCOTE
                AND  B.CTARIF_COMPO_PCOTE
                                        = :GFCTB029.CTARIF-COMPO-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -811) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ADESAO AO PACOTE - ERRO NO DISTINCT DAS TABELAS GFCTB029 E
      *--- GFCTB009
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1746'         TO GFCTG2-COD-MSG
                   MOVE 1746           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1537'         TO GFCTG2-COD-MSG
                   MOVE 1537           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA

               MOVE 04                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF (SQLCODE                 EQUAL ZEROS OR -811)
               MOVE 'S'                TO WRK-EXISTE-COMPONENTE
           END-IF.

           PERFORM 2530-LER-CURSOR.

      *----------------------------------------------------------------*
       2510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ABRIR CURSOR DA TABELA GFCTB029.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2520-ABRIR-CURSOR               SECTION.
      *----------------------------------------------------------------*

           MOVE CMTFWY6I-E-CD-CESTA-PERM TO CTARIF-PCOTE OF GFCTB029.

           EXEC SQL
               OPEN  CSR01-GFCTB029
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ADESAO AO PACOTE - TABELA GFCTB029 INDISPONIVEL
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1747'         TO GFCTG2-COD-MSG
                   MOVE 1747           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1535'         TO GFCTG2-COD-MSG
                   MOVE 1535           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 04                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * LER   CURSOR DA TABELA GFCTB029.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2530-LER-CURSOR                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              FETCH
                     CSR01-GFCTB029
               INTO
                    :GFCTB029.CTARIF-COMPO-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ADESAO AO PACOTE - TABELA GFCTB029 INDISPONIVEL
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1748'         TO GFCTG2-COD-MSG
                   MOVE 1748           TO  CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1536'         TO GFCTG2-COD-MSG
                   MOVE 1536           TO  CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 04                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR
           END-IF.

      *----------------------------------------------------------------*
       2530-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * FECHAR CURSOR DA TABELA GFCTB029.                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2540-FECHAR-CURSOR              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              CLOSE  CSR01-GFCTB029
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ADESAO AO PACOTE - TABELA GFCTB029 INDISPONIVEL
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1744'         TO GFCTG2-COD-MSG
                   MOVE 1744           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1538'         TO GFCTG2-COD-MSG
                   MOVE 1538           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA

               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2540-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CALCULAR A DATA DE INICIO DA ADESAO INDIVIDUAL                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2600-CALCULAR-DATA-INI          SECTION.
      *----------------------------------------------------------------*

           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-PROCM-ATUAL.
           MOVE WRK-MES-DB2            TO WRK-MES-PROCM-ATUAL.
           MOVE WRK-ANO-DB2            TO WRK-ANO-PROCM-ATUAL.

           MOVE DINIC-PRMSS-PCOTE OF GFCTB0C6
                                       TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-INI-PRMSS-PCOTE.
           MOVE WRK-MES-DB2            TO WRK-MES-INI-PRMSS-PCOTE.
           MOVE WRK-ANO-DB2            TO WRK-ANO-INI-PRMSS-PCOTE.

           IF  WRK-DT-INI-PRMSS-PCOTE  GREATER WRK-DT-PROCM-ATUAL
               MOVE WRK-DT-INI-PRMSS-PCOTE
                                       TO WRK-DT-INI-ADSAO-INDVD

               IF  WRK-DIA-INI-ADSAO-INDVD
                                       NOT EQUAL 01
                   MOVE 01             TO WRK-DIA-INI-ADSAO-INDVD

                   MOVE WRK-MES-INI-ADSAO-INDVD
                                       TO WRK-MES
                   ADD  1              TO WRK-MES
                   MOVE WRK-MES        TO WRK-MES-INI-ADSAO-INDVD

                   IF  WRK-MES-INI-ADSAO-INDVD
                                       GREATER 12
                       MOVE 01         TO WRK-MES-INI-ADSAO-INDVD

                       MOVE WRK-ANO-INI-ADSAO-INDVD
                                       TO WRK-ANO
                       ADD  1          TO WRK-ANO
                       MOVE WRK-ANO    TO WRK-ANO-INI-ADSAO-INDVD
                   END-IF
               END-IF
           ELSE
               MOVE WRK-DT-PROCM-ATUAL TO WRK-DT-INI-ADSAO-INDVD
               MOVE 01                 TO WRK-DIA-INI-ADSAO-INDVD

               MOVE WRK-MES-INI-ADSAO-INDVD
                                       TO WRK-MES
               ADD  1                  TO WRK-MES
               MOVE WRK-MES            TO WRK-MES-INI-ADSAO-INDVD

               IF  WRK-MES-INI-ADSAO-INDVD
                                       GREATER 12
                   MOVE 01             TO WRK-MES-INI-ADSAO-INDVD

                   MOVE WRK-ANO-INI-ADSAO-INDVD
                                       TO WRK-ANO
                   ADD  1              TO WRK-ANO
                   MOVE WRK-ANO        TO WRK-ANO-INI-ADSAO-INDVD
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CALCULAR A DATA DE FIM DA ADESAO INDIVIDUAL.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2700-CALCULAR-DATA-FIM          SECTION.
      *----------------------------------------------------------------*

           PERFORM 2720-OBTER-DADOS-GFCTB0A2.

           MOVE DFIM-VGCIA-TARIF OF GFCTB0A2
                                       TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-FIM-ADSAO-INDVD.
           MOVE WRK-MES-DB2            TO WRK-MES-FIM-ADSAO-INDVD.
           MOVE WRK-ANO-DB2            TO WRK-ANO-FIM-ADSAO-INDVD.

           IF (WRK-MES-FIM-ADSAO-INDVD EQUAL 01 OR 03 OR 05 OR 07 OR
                                             08 OR 10 OR 12) AND
               WRK-DIA-FIM-ADSAO-INDVD EQUAL 31
               NEXT SENTENCE
           ELSE
               IF (WRK-MES-FIM-ADSAO-INDVD
                                       EQUAL 04 OR 06 OR 09 OR 11) AND
                   WRK-DIA-FIM-ADSAO-INDVD
                                       EQUAL 30
                   NEXT SENTENCE
               ELSE
                   IF  WRK-MES-FIM-ADSAO-INDVD
                                       EQUAL 02 AND
                       WRK-DIA-FIM-ADSAO-INDVD
                                       EQUAL 29
                       NEXT SENTENCE
                   ELSE
                       MOVE WRK-ANO-FIM-ADSAO-INDVD
                                       TO WRK-ANO

                       DIVIDE WRK-ANO  BY 4
                                       GIVING WRK-INTEIRO
                                       REMAINDER WRK-RESTO

                       MOVE WRK-ANO    TO WRK-ANO-FIM-ADSAO-INDVD

                       IF  WRK-MES-FIM-ADSAO-INDVD
                                       EQUAL 02 AND
                           WRK-RESTO   NOT EQUAL ZEROS AND
                           WRK-DIA-FIM-ADSAO-INDVD
                                       EQUAL 28
                           NEXT SENTENCE
                       ELSE
                           PERFORM 2710-ACHAR-DATA-FIM
                       END-IF
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACHAR A DATA DE FIM DA ADESAO INDIVIDUAL.                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2710-ACHAR-DATA-FIM             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MES-FIM-ADSAO-INDVD
                                       TO WRK-MES.
           SUBTRACT 1                  FROM WRK-MES.
           MOVE WRK-MES                TO WRK-MES-FIM-ADSAO-INDVD.

           EVALUATE WRK-MES-FIM-ADSAO-INDVD

           WHEN 00

                MOVE 31                TO WRK-DIA-FIM-ADSAO-INDVD
                MOVE 12                TO WRK-MES-FIM-ADSAO-INDVD

                MOVE WRK-ANO-FIM-ADSAO-INDVD
                                       TO WRK-ANO
                SUBTRACT 1             FROM WRK-ANO
                MOVE WRK-ANO           TO WRK-ANO-FIM-ADSAO-INDVD

           WHEN 01
           WHEN 03
           WHEN 05
           WHEN 07
           WHEN 08
           WHEN 10
           WHEN 12

                MOVE 31                TO WRK-DIA-FIM-ADSAO-INDVD

           WHEN 04
           WHEN 06
           WHEN 09
           WHEN 11

                MOVE 30                TO WRK-DIA-FIM-ADSAO-INDVD

           WHEN 02

                IF  WRK-RESTO          EQUAL ZEROS
                    MOVE 28            TO WRK-DIA-FIM-ADSAO-INDVD
                ELSE
                    MOVE 29            TO WRK-DIA-FIM-ADSAO-INDVD
                END-IF

           END-EVALUATE.

      *----------------------------------------------------------------*
       2710-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER DADOS DO PARAMETRO DA TARIFA.                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2720-OBTER-DADOS-GFCTB0A2       SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0C6
                                       TO CSERVC-TARIF OF GFCTB0A2.

           EXEC SQL
             SELECT
                     MAX(DFIM_VGCIA_TARIF)
               INTO
                    :GFCTB0A2.DFIM-VGCIA-TARIF
               FROM
                     DB2PRD.PARM_SERVC_TARIF
              WHERE
                     CSERVC_TARIF       = :GFCTB0A2.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0A2
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1722'         TO GFCTG2-COD-MSG
                   MOVE 1722           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1512'         TO GFCTG2-COD-MSG
                   MOVE 1512           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2720-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * MOVIMENTAR AS INFORMACOES PARA AREA DE SAIDA.                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2800-INSERIR-GFCTB009           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                SET :WRK-HINCL-REG      = CURRENT TIMESTAMP
           END-EXEC.

           MOVE CMTFWY6I-E-CD-COML-AGENCIA TO CJUNC-DEPDC   OF GFCTB009.
           MOVE CMTFWY6I-E-CD-COML-CONTA   TO CCTA-CLI      OF GFCTB009.
           MOVE CMTFWY6I-E-CD-CESTA-PERM   TO CSERVC-TARIF  OF GFCTB009.

           MOVE WRK-HINCL-REG          TO HINCL-REG         OF GFCTB009.

           MOVE 000006051              TO CFUNC-MANUT-INCL  OF GFCTB009.
           MOVE ZEROS                  TO CFUNC-MANUT       OF GFCTB009.

           MOVE '01.01.0001'           TO WRK-DATA-DB2.
           MOVE WRK-DIA-INI-ADSAO-INDVD
                                       TO WRK-DIA-DB2.
           MOVE WRK-MES-INI-ADSAO-INDVD
                                       TO WRK-MES-DB2.
           MOVE WRK-ANO-INI-ADSAO-INDVD
                                       TO WRK-ANO-DB2.
           MOVE WRK-DATA-DB2           TO DINIC-ADSAO-INDVD OF GFCTB009.

           MOVE '01.01.0001'           TO WRK-DATA-DB2.
           MOVE WRK-DIA-FIM-ADSAO-INDVD
                                       TO WRK-DIA-DB2.
           MOVE WRK-MES-FIM-ADSAO-INDVD
                                       TO WRK-MES-DB2.
           MOVE WRK-ANO-FIM-ADSAO-INDVD
                                       TO WRK-ANO-DB2.
           MOVE WRK-DATA-DB2           TO DFIM-ADSAO-INDVD  OF GFCTB009.

           MOVE '0001-01-01-01.01.01.000001'
                                       TO HMANUT-REG        OF GFCTB009.
           MOVE ZEROS                  TO CINDCD-EXCL-REG   OF GFCTB009.

           EXEC SQL
             INSERT
               INTO
                      DB2PRD.ADSAO_INDVD_PCOTE
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
                     :WRK-HINCL-REG,
                     :GFCTB009.CFUNC-MANUT-INCL,
                     :GFCTB009.CFUNC-MANUT,
                     :GFCTB009.DINIC-ADSAO-INDVD,
                     :GFCTB009.DFIM-ADSAO-INDVD,
                     :GFCTB009.HMANUT-REG,
                     :GFCTB009.CINDCD-EXCL-REG)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -803) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0C9
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1742'         TO GFCTG2-COD-MSG
                   MOVE 1742           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1533'         TO GFCTG2-COD-MSG
                   MOVE 1533           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA

               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -803
               MOVE 08                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ADESAO AO PACOTE - REGISTRO JA EXISTE NA TABELA GFCTB009
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1741'         TO GFCTG2-COD-MSG
                   MOVE 1741           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1532'         TO GFCTG2-COD-MSG
                   MOVE 1532           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA

               MOVE 04                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * REMOVER AS INFORMACOES PARA AREA DE SAIDA.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2900-EXCLUIR-GFCTB009           SECTION.
      *----------------------------------------------------------------*

           MOVE CMTFWY6I-E-CD-COML-AGENCIA TO CJUNC-DEPDC  OF GFCTB009.
           MOVE CMTFWY6I-E-CD-COML-CONTA   TO CCTA-CLI     OF GFCTB009.
           MOVE CMTFWY6I-E-CD-CESTA-PERM   TO CSERVC-TARIF OF GFCTB009.

           MOVE WRK-HINCL-REG          TO HINCL-REG        OF GFCTB009.

           EXEC SQL
             DELETE
               FROM
                     DB2PRD.ADSAO_INDVD_PCOTE
              WHERE
                     CJUNC_DEPDC       = :GFCTB009.CJUNC-DEPDC
                AND  CCTA_CLI          = :GFCTB009.CCTA-CLI
                AND  CSERVC_TARIF      = :GFCTB009.CSERVC-TARIF
                AND  HINCL_REG         = :GFCTB009.HINCL-REG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0C9
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1742'         TO GFCTG2-COD-MSG
                   MOVE 1742           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1533'         TO GFCTG2-COD-MSG
                   MOVE 1533           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA

               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * REMOVER AS INFORMACOES PARA AREA DE SAIDA.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2950-EXCLUIR-GFCTB0M2           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             DELETE
               FROM
                     DB2PRD.TADSAO_INDVD_CLULR
              WHERE
                     CAG_BCRIA         = :GFCTB0M2.CAG-BCRIA
                AND  CCTA_CLI          = :GFCTB0M2.CCTA-CLI
                AND  CSERVC_TARIF      = :GFCTB0M2.CSERVC-TARIF
                AND  HINCL_REG         = :WRK-HINCL-REG
                AND  NSEQ_REG_CLULR    = :GFCTB0M2.NSEQ-REG-CLULR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0M2
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1794'         TO GFCTG2-COD-MSG
                   MOVE 1794           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1542'         TO GFCTG2-COD-MSG
                   MOVE 1542           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA

               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2950-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * MOVIMENTAR AS INFORMACOES PARA AREA DE SAIDA.                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-INSERIR-GFCTB0M2           SECTION.
      *----------------------------------------------------------------*

           MOVE CMTFWY6I-E-CD-COML-AGENCIA TO CAG-BCRIA    OF GFCTB0M2.
           MOVE CMTFWY6I-E-CD-COML-CONTA   TO CCTA-CLI     OF GFCTB0M2.
           MOVE CMTFWY6I-E-CD-CESTA-PERM   TO CSERVC-TARIF OF GFCTB0M2.

           MOVE 1                      TO NSEQ-REG-CLULR   OF GFCTB0M2.

           MOVE WRK-HINCL-REG          TO HINCL-REG        OF GFCTB009.

           MOVE CMTFWY6I-E-OPERADORA   TO COPER-FONE-MOVEL OF GFCTB0M2.
           MOVE CMTFWY6I-E-DDD         TO WRK-NUM04-A.
           MOVE WRK-NUM03-N            TO CDDD-FONE-MOVEL  OF GFCTB0M2.
           MOVE CMTFWY6I-E-NUMERO      TO NLIN-TFONI       OF GFCTB0M2.

           MOVE 000006051              TO CFUNC-INCL-SIST  OF GFCTB0M2.

           MOVE ZEROS                  TO CFUNC-MANUT      OF GFCTB0M2.

           MOVE '0001-01-01-01.01.01.000001'
                                       TO HMANUT-REG       OF GFCTB0M2.

           EXEC SQL
             INSERT
               INTO
                      DB2PRD.TADSAO_INDVD_CLULR
                     (CAG_BCRIA,
                      CCTA_CLI,
                      CSERVC_TARIF,
                      HINCL_REG,
                      NSEQ_REG_CLULR,
                      COPER_FONE_MOVEL,
                      CDDD_FONE_MOVEL,
                      CFUNC_INCL_SIST,
                      CFUNC_MANUT,
                      HMANUT_REG,
                      NLIN_TFONI)
             VALUES
                    (:GFCTB0M2.CAG-BCRIA,
                     :GFCTB0M2.CCTA-CLI,
                     :GFCTB0M2.CSERVC-TARIF,
                     :WRK-HINCL-REG,
                     :GFCTB0M2.NSEQ-REG-CLULR,
                     :GFCTB0M2.COPER-FONE-MOVEL,
                     :GFCTB0M2.CDDD-FONE-MOVEL,
                     :GFCTB0M2.CFUNC-INCL-SIST,
                     :GFCTB0M2.CFUNC-MANUT,
                     :GFCTB0M2.HMANUT-REG,
                     :GFCTB0M2.NLIN-TFONI)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0M2
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1794'         TO GFCTG2-COD-MSG
                   MOVE 1794           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1542'         TO GFCTG2-COD-MSG
                   MOVE 1542           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9800-OBTER-DESC-MSG-S-FIN

               IF  CMTFWY6I-COD-RETORNO EQUAL 12
                   MOVE GFCTG3-DESC-MSG
                                       TO CMTFWY6I-MENSAGEM
                   MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA)
                                       TO CMTFWY6I-SQLCA

                   MOVE 12             TO RETURN-CODE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR PERMISSAO DE ADESAO GFCTB0N7
      ******************************************************************
      *----------------------------------------------------------------*
       3105-VERIFICA-PERMISSAO-ADESAO        SECTION.
      *----------------------------------------------------------------*
           MOVE CMTFWY6I-E-CD-CESTA-PERM
                                     TO CSERVC-TARIF     OF GFCTB0N7.
           MOVE CMTFWY6I-E-OPERADORA
                                     TO COPER-FONE-MOVEL OF GFCTB0N7.
           MOVE CMTFWY6I-E-DDD       TO WRK-NUM04-A.
           MOVE WRK-NUM03-N          TO CDDD-FONE-MOVEL  OF GFCTB0N7.
           MOVE DPROCM-ATUAL                             OF GFCTB0A1 TO
                DINIC-VGCIA-REG                          OF GFCTB0N7
                DFIM-VGCIA-REG                           OF GFCTB0N7.

           EXEC SQL
             SELECT
                   QMAX_DIG_MOVEL
             INTO
                   :GFCTB0N7.QMAX-DIG-MOVEL
             FROM   DB2PRD.TOPER_PRMSS_CLULR
             WHERE CSERVC_TARIF       = :GFCTB0N7.CSERVC-TARIF
             AND   COPER_FONE_MOVEL   = :GFCTB0N7.COPER-FONE-MOVEL
             AND   CDDD_FONE_MOVEL    = :GFCTB0N7.CDDD-FONE-MOVEL
             AND   DINIC_VGCIA_REG    <= :GFCTB0N7.DINIC-VGCIA-REG
             AND   DFIM_VGCIA_REG     >= :GFCTB0N7.DFIM-VGCIA-REG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
               MOVE '1856'             TO GFCTG2-COD-MSG
               MOVE 1856                TO CMTFWY6I-COD-MENSAGEM
               PERFORM 9800-OBTER-DESC-MSG-S-FIN
               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 12             TO RETURN-CODE
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
               MOVE '1857'             TO GFCTG2-COD-MSG
               MOVE 1857                TO CMTFWY6I-COD-MENSAGEM
               PERFORM 9800-OBTER-DESC-MSG-S-FIN
               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 4                  TO RETURN-CODE
               PERFORM 9999-FINALIZAR
            END-IF.

            IF (WRK-NUM09-D     EQUAL ZEROS     AND
                QMAX-DIG-MOVEL OF GFCTB0N7 = 8) OR
               (WRK-NUM08-D NOT EQUAL ZEROS     AND
                QMAX-DIG-MOVEL OF GFCTB0N7 = 9)
                NEXT SENTENCE
            ELSE
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
               MOVE '1858'             TO GFCTG2-COD-MSG
               MOVE 1858                TO CMTFWY6I-COD-MENSAGEM
               PERFORM 9800-OBTER-DESC-MSG-S-FIN
               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SQLCA(1:LENGTH OF CMTFWY6I-SQLCA) TO CMTFWY6I-SQLCA
               MOVE 4                  TO RETURN-CODE
               PERFORM 9999-FINALIZAR
            END-IF.

      *----------------------------------------------------------------*
       3105-99-FIM.                    EXIT.
      *----------------------------------------------------------------*



      ******************************************************************
      * ROTINA P/ ACESSO AO MODULO DE DESCRICOES DE MSGS S/ FINALIZAR  *
      ******************************************************************
      *----------------------------------------------------------------*
       9800-OBTER-DESC-MSG-S-FIN       SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE 'GFCT6046'             TO GFCTG2-TRANSACAO
                                          GFCTG2-FUNCAO.
           MOVE '1'                    TO GFCTG2-FUNC-BDSCO.
           MOVE CMTFWY6I-E-AMBIENTE    TO GFCTG2-TIPO-PROC.
           MOVE 'N'                    TO GFCTG2-FIM.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- NAO FOI POSSIVEL OBTER A MENSAGEM NO MODULO GFCT5523
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE 1803           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE 1593           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               MOVE WRK-MSG-1          TO CMTFWY6I-MENSAGEM

               MOVE 12                 TO RETURN-CODE

               MOVE SPACES             TO CMTFWY6I-SQLCA

               MOVE 04                 TO RETURN-CODE

               GO                      TO 9800-99-FIM
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- PROBLEMA NO ACESSO A BASE DE MENSAGENS
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE 1804           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE 1594           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               MOVE WRK-MSG-2          TO CMTFWY6I-MENSAGEM

               MOVE SPACES             TO CMTFWY6I-SQLCA

               MOVE 12                 TO RETURN-CODE

               GO                      TO 9800-99-FIM
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- MENSAGEM NAO ENCONTRADA NA BASE DE MENSAGENS
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE 1805           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE 1595           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               MOVE WRK-MSG-3          TO CMTFWY6I-MENSAGEM

               MOVE SPACES             TO CMTFWY6I-SQLCA

               MOVE 12                 TO RETURN-CODE
           END-IF.

      *----------------------------------------------------------------*
       9800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA ACESSO AO MODULO DE DESCRICOES DE MENSAGENS.       *
      ******************************************************************
      *----------------------------------------------------------------*
       9900-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE 'GFCT6046'             TO GFCTG2-TRANSACAO
                                          GFCTG2-FUNCAO.
           MOVE '1'                    TO GFCTG2-FUNC-BDSCO.
           MOVE CMTFWY6I-E-AMBIENTE    TO GFCTG2-TIPO-PROC.
           MOVE 'N'                    TO GFCTG2-FIM.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- NAO FOI POSSIVEL OBTER A MENSAGEM NO MODULO GFCT5523
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE 1803           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE 1593           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               MOVE WRK-MSG-1          TO CMTFWY6I-MENSAGEM

               MOVE 12                 TO RETURN-CODE

               MOVE SPACES             TO CMTFWY6I-SQLCA

               MOVE 04                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- PROBLEMA NO ACESSO A BASE DE MENSAGENS
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE 1804           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE 1594           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               MOVE WRK-MSG-2          TO CMTFWY6I-MENSAGEM

               MOVE SPACES             TO CMTFWY6I-SQLCA

               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- MENSAGEM NAO ENCONTRADA NA BASE DE MENSAGENS
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE 1805           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE 1595           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               MOVE WRK-MSG-3          TO CMTFWY6I-MENSAGEM

               MOVE SPACES             TO CMTFWY6I-SQLCA

               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       9900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA TRATAR RETORNO DO MODULO GFCT5307               *
      ******************************************************************
      *----------------------------------------------------------------*
       9950-OBTEM-MSG-GFCT5307 SECTION.
      *----------------------------------------------------------------*

           IF  WAN7-COD-RETORNO  EQUAL  4
               MOVE 'B'                TO CMTFWY6I-E-AMBIENTE

               MOVE 08                 TO CMTFWY6I-COD-RETORNO
      *
      *--- DADOS ENVIADOS AO MODULO GFCT5307 INCONSISTENTES
      *
               MOVE '1501'             TO GFCTG2-COD-MSG
               MOVE 1501               TO CMTFWY6I-COD-MENSAGEM

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE SPACES             TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  WAN7-COD-RETORNO  EQUAL  7
               MOVE 08                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ADESAO AO PACOTE - ERRO NA TABELA DB2PRD.ADSAO_INDVD_PCOTE
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1742'         TO GFCTG2-COD-MSG
                   MOVE 1742           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1533'         TO GFCTG2-COD-MSG
                   MOVE 1533           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE WAN7-SQLCA         TO CMTFWY6I-SQLCA

               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  WAN7-COD-RETORNO  EQUAL  9
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0A1
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1717'         TO GFCTG2-COD-MSG
                   MOVE 1717           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1507'         TO GFCTG2-COD-MSG
                   MOVE 1507           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE WAN7-SQLCA         TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  WAN7-COD-RETORNO  EQUAL  11
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0C6
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1739'         TO GFCTG2-COD-MSG
                   MOVE 1739           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1530'         TO GFCTG2-COD-MSG
                   MOVE 1530           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE WAN7-SQLCA         TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  WAN7-COD-RETORNO  EQUAL  13
               MOVE 12                 TO CMTFWY6I-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0M2
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1794'         TO GFCTG2-COD-MSG
                   MOVE 1794           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1542'         TO GFCTG2-COD-MSG
                   MOVE 1542           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE WAN7-SQLCA         TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  WAN7-COD-RETORNO  EQUAL  15
               MOVE 08                 TO CMTFWY6I-COD-RETORNO
      *
      *--- CELULAR INFORMADO JA ADERIDO A UMA CESTA/CLIENTE
      *
               IF  CMTFWY6I-E-AMBIENTE EQUAL 'O'
                   MOVE '1796'         TO GFCTG2-COD-MSG
                   MOVE 1796           TO CMTFWY6I-COD-MENSAGEM
               ELSE
                   MOVE '1546'         TO GFCTG2-COD-MSG
                   MOVE 1546           TO CMTFWY6I-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE WAN7-SQLCA         TO CMTFWY6I-SQLCA
               MOVE 04                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  WAN7-COD-RETORNO  EQUAL  17
               MOVE 'B'                TO CMTFWY6I-E-AMBIENTE

               MOVE 08                 TO CMTFWY6I-COD-RETORNO
      *
      *--- DADOS ENVIADOS AO MODULO GFCT5307 INCONSISTENTES
      *
               MOVE '1501'             TO GFCTG2-COD-MSG
               MOVE 1501               TO CMTFWY6I-COD-MENSAGEM

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO CMTFWY6I-MENSAGEM
               MOVE WAN7-SQLCA         TO CMTFWY6I-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       9950-OBTEM-MSG-GFCT5307-9-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA FINALIZACAO DO PROGRAMA.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


