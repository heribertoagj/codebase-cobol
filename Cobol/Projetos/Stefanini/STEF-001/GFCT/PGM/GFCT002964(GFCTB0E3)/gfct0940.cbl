      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0940.
       AUTHOR. GIORGIA L. B. BORSATTI.
      *================================================================*
      *                    C P M   S I S T E M A S*
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT0940                                     *
      *    PROGRAMADORA : GIORGIA L. B. BORSATTI        -  CPM/FPOLIS  *
      *    ANALISTA CPM : MARCIO RODRIGO DA CUNHA       -  CPM/FPOLIS  *
      *    ANALISTA     : VANI NUNES - PROCWORK         -  GRUPO 50    *
      *    DATA         : 19/01/2007                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *     FORMATACAO DA TELA DE LISTA DE TARIFAS SUSPENSAS POR       *
      *     AGRUPAMENTO.                                               *
      *                                                                *
      *    BANCO DE DADOS :                                            *
      *     DB2                                                        *
      *      TABLE                                 INCLUDE/BOOK        *
      *      DB2PRD.SUSP_GRP_CLI                     GFCTB0E3          *
      *      DB2PRD.SUSP_GRP_DEPDC                   GFCTB0E4          *
      *      DB2PRD.SUSP_GRP_MUN                     GFCTB0E5          *
      *      DB2PRD.SUSP_GRP_UF                      GFCTB0E6          *
      *      DB2PRD.SUSP_TARIF_AGPTO                 GFCTB0E7          *
      *      DB2PRD.SUSP_TARIF_CTA                   GFCTB0E8          *
      *      DB2PRD.SUSP_GRP_PAB                     GFCTB0I1          *
      *      DB2PRD.PARM_DATA_PROCM                  GFCTB0A1          *
      *      DB2PRD.SERVC_TARIF_PRINC                GFCTB0D8          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                 *
      *      GFCT5522 - VERIFICA DISPONIBILIDADE ONLINE                *
      *      GFCT5523 - OBTER DESCRICAO DE MENSAGENS                   *
      *      GFCT5537 - VERIFICA PERMISSAO DO USUSARIO                 *
      *      MESU9015 - OBTER DESCRICAO AGENCIA                        *
      *      MESU9018 - OBTER DESCRICAO PAB                            *
      *      RURC9020 - OBTER DESCRICAO MUNICIPIO                      *
      *      DCIT8000 - VERIFICA UF                                    *
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
       77  FILLER                      PIC  X(050)         VALUE
           '*** GFCT0940 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA INDEXADORES ***'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(003) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-FIM-CURSOR              PIC  X(001)         VALUE 'N'.
       77  WRK-QTDE-OCOR               PIC  9(003) COMP-3  VALUE ZEROS.
       77  WRK-SQLCODE                 PIC S9(009) COMP    VALUE ZEROS.

       01  WRK-TARIFA-INI              PIC S9(005) COMP-3   VALUE ZEROS.
       01  WRK-TARIFA-FIM              PIC S9(005) COMP-3   VALUE ZEROS.
       01  WRK-COD-AGPTO-FIM           PIC S9(003)V COMP-3  VALUE ZEROS.
       01  WRK-CSEQ-AGPTO              PIC S9(009)V COMP-3  VALUE ZEROS.

       01  WRK-COUNT                   PIC S9(009) COMP-3   VALUE ZEROS.

       01  WRK-AGENCIA-INI             PIC S9(005)V COMP-3  VALUE ZEROS.
       01  WRK-AGENCIA-FIM             PIC S9(005)V COMP-3  VALUE ZEROS.

       01  WRK-NOME-TABELA.
           05  FILLER                  PIC  X(003)         VALUE ' - '.
           05  WRK-NOME-TAB            PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE MENSAGENS ***'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGENS.
           03  WRK-MSG05.
               05  FILLER              PIC  X(026)         VALUE
                 'ERRO NA CHAMADA AO MODULO '.
               05  WRK-NOME-MOD        PIC  X(008)         VALUE SPACES.
               05  FILLER              PIC  X(041)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VARIAVEIS DE DATAS ***'.
      *----------------------------------------------------------------*

       01  WRK-DPROCM-ATUAL            PIC  X(010)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM DCIT8000  *'.
      *----------------------------------------------------------------*

       01  WRK-DCIT-UNIDADE-FEDERATIVA.
           05 WRK-AREA-DCLGEN-DCITV002 PIC  X(070)         VALUE SPACES.
           05 WRK-DCIT-CODIGO-RETORNO  PIC  9(002)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA ENTRADA DO MODULO GFCT5522 ***'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTFZ'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA SAIDA DO MODULO GFCT5522 ***'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA ENTRADA DO MODULO GFCT5523 ***'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG2'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA SAIDA DO MODULO GFCT5523 ***'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA ENTRADA DO MODULO GFCT5537 ***'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTKN'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA SAIDA DO MODULO GFCT5537 ***'.
      *----------------------------------------------------------------*
       COPY 'I#GFCTKO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO - RURC9020 **'.
      *----------------------------------------------------------------*

       COPY 'I#RURC88'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO - MESU9018 **'.
      *----------------------------------------------------------------*

       COPY 'I#MESUM8'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO - MESU9015 **'.
      *----------------------------------------------------------------*

       COPY 'I#MESUM6'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0E3
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0E4
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0E5
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0E6
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0E7
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0E8
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0I1
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
               INCLUDE DCITV002
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA DECLARACAO DO CURSOR 1 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
            DECLARE  CSR01-GFCTB0E7 CURSOR FOR
             SELECT  CSERVC_TARIF,
                     CSEQ_AGPTO_CTA,
TESTE                DINIC_SUSP_COBR,
                     DFIM_SUSP_COBR,
TESTE                CINDCD_AGPTO_TOT
               FROM  DB2PRD.SUSP_TARIF_AGPTO
              WHERE  CAGPTO_CTA         = :GFCTB0E7.CAGPTO-CTA
                AND (CSERVC_TARIF      >= :WRK-TARIFA-INI
                AND  CSERVC_TARIF      <= :WRK-TARIFA-FIM)
TESTE *---      AND  CSEQ_AGPTO_CTA    >= :WRK-CSEQ-AGPTO
                AND  DFIM_SUSP_COBR    <= :WRK-DPROCM-ATUAL
                AND  CINDCD_LIBRC_COBR IN ('N', 'C')
           ORDER BY  CSERVC_TARIF,
                     CSEQ_AGPTO_CTA ASC,
                     DFIM_SUSP_COBR DESC
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** GFCT0940 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTY0'.

       COPY 'I#GFCTY1'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE DIVISION              USING GFCTY0-ENTRADA
                                             GFCTY1-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
      * ROTINA PRINCIPAL                                               *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 0100-INICIALIZAR.

           PERFORM 1100-PROCESSAR.

           PERFORM 9000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
      * ROTINA DE PROCEDIMENTOS INICIAIS                               *
      *----------------------------------------------------------------*
       0100-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM-CURSOR.
           MOVE ZEROS                  TO WRK-SQLCODE.

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          GFCTY1-SAIDA
                                          GFCTG2-ENTRADA.

           INITIALIZE GFCT0M-AREA-ERROS
                      GFCTY1-SAIDA
                      GFCTG2-ENTRADA.

           PERFORM 0200-VERIFICAR-SISTEMA-DISP.

           PERFORM 0300-MONTAR-AREA-SAIDA.

           PERFORM 0900-OBTER-DATA-PROC.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * VERIFICAR DISPONIBILIDADE DO SISTEMA                           *
      *----------------------------------------------------------------*
       0200-VERIFICAR-SISTEMA-DISP     SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO  GFCTFZ-ENTRADA

           INITIALIZE                  GFCTFZ-ENTRADA
                                       GFCTG1-SAIDA.

           MOVE 100                    TO GFCTFZ-LL.
           MOVE ZEROS                  TO GFCTFZ-ZZ.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE GFCTY0-TRANSACAO       TO GFCTFZ-TRANSACAO.
           MOVE GFCTY0-FUNCAO          TO GFCTFZ-FUNCAO.
           MOVE GFCTY0-FUNC-BDSCO      TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.

           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0940'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTY1-ERRO

               PERFORM 9000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 9
               MOVE 'GFCT0940'         TO GFCT0M-TRANSACAO
               MOVE GFCTG1-COD-SQL-ERRO
                                       TO GFCT0M-SQLCODE
                                          GFCTY1-COD-SQL-ERRO
               MOVE GFCTG1-ERRO        TO GFCTY1-ERRO
               MOVE GFCTG1-COD-MSG-ERRO
                                       TO GFCTY1-COD-MSG-ERRO
               MOVE GFCTG1-DESC-MSG-ERRO
                                       TO GFCTY1-DESC-MSG-ERRO
                                          GFCT0M-TEXTO

               PERFORM 9000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 'SISTEMA INDISPONIVEL'
                                       TO GFCT0M-TEXTO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0940'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTY1-ERRO

               PERFORM 9000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * MONTA AREA DE SAIDA                                            *
      *----------------------------------------------------------------*
       0300-MONTAR-AREA-SAIDA          SECTION.
      *----------------------------------------------------------------*

           MOVE +296                   TO GFCTY1-LL.
           MOVE ZEROS                  TO GFCTY1-ZZ.

           MOVE GFCTY0-TRANSACAO       TO GFCTY1-TRANSACAO.
           MOVE GFCTY0-FUNCAO          TO GFCTY1-FUNCAO.
           MOVE GFCTY0-AGPTO           TO GFCTY1-AGPTO.
           MOVE GFCTY0-TARIFA          TO GFCTY1-TARIFA.
           MOVE GFCTY0-PONT-CSEQ-AGPTO TO GFCTY1-PONT-CSEQ-AGPTO.
           MOVE GFCTY0-PONT-TARIFA     TO GFCTY1-PONT-TARIFA.
           MOVE GFCTY0-PONT-AGENCIA    TO GFCTY1-PONT-AGENCIA.
           MOVE GFCTY0-PONT-POSTO      TO GFCTY1-PONT-POSTO.
           MOVE GFCTY0-PONT-FILIAL     TO GFCTY1-PONT-FILIAL.
           MOVE GFCTY0-PONT-CONTROLE   TO GFCTY1-PONT-CONTROLE.
           MOVE GFCTY0-PONT-COD-MUNIC  TO GFCTY1-PONT-COD-MUNIC.
           MOVE GFCTY0-PONT-UF         TO GFCTY1-PONT-UF.
           MOVE GFCTY0-PONT-DATA-SUSP  TO GFCTY1-PONT-DATA-SUSP.

           IF  GFCTY0-QTDE-OCOR        NOT NUMERIC
               MOVE ZEROS               TO GFCTY1-QTDE-OCOR
           ELSE
               MOVE GFCTY0-QTDE-OCOR    TO GFCTY1-QTDE-OCOR
           END-IF.

           MOVE ZEROS                   TO  GFCTY1-QTDE-TOT-REG.

           MOVE GFCTY0-FUNC-BDSCO       TO GFCTY1-FUNC-BDSCO.
           MOVE 'N'                     TO GFCTY1-FIM.

           MOVE '0000'                 TO GFCTG2-COD-MSG.

           PERFORM 0600-OBTER-DESCRICAO-MSG.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * CONSITIR CAMPOS RECEBIDOS                                      *
      *----------------------------------------------------------------*
       0400-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (GFCTY0-TRANSACAO        EQUAL SPACES OR LOW-VALUES)
               MOVE  1123              TO GFCTY1-COD-MSG-ERRO
               MOVE '1123'             TO GFCTG2-COD-MSG

               PERFORM 0500-FORMATAR-COD-ERRO
           END-IF.

           IF (GFCTY0-FUNCAO           EQUAL SPACES OR LOW-VALUES)
               MOVE  1124              TO GFCTY1-COD-MSG-ERRO
               MOVE '1124'             TO GFCTG2-COD-MSG

               PERFORM 0500-FORMATAR-COD-ERRO
           END-IF.

           IF (GFCTY0-TARIFA           NOT NUMERIC) OR
              (GFCTY0-PONT-TARIFA      NOT NUMERIC)
               MOVE  1125              TO GFCTY1-COD-MSG-ERRO
               MOVE '1125'             TO GFCTG2-COD-MSG

               PERFORM 0500-FORMATAR-COD-ERRO
           END-IF.

           IF (GFCTY0-AGPTO            NOT NUMERIC OR
               GFCTY0-AGPTO            NOT EQUAL 4 AND 14 AND 15 AND
                                                18 AND 19)
               MOVE  1200              TO GFCTY1-COD-MSG-ERRO
               MOVE '1200'             TO GFCTG2-COD-MSG

               PERFORM 0500-FORMATAR-COD-ERRO
           END-IF.

           IF (GFCTY0-PONT-CSEQ-AGPTO  NOT NUMERIC)
               MOVE  1201              TO GFCTY1-COD-MSG-ERRO
               MOVE '1201'             TO GFCTG2-COD-MSG

               PERFORM 0500-FORMATAR-COD-ERRO
           END-IF.

           IF (GFCTY0-PONT-AGENCIA     NOT NUMERIC)
               MOVE  1185              TO GFCTY1-COD-MSG-ERRO
               MOVE '1185'             TO GFCTG2-COD-MSG

               PERFORM 0500-FORMATAR-COD-ERRO
           END-IF.

           IF (GFCTY0-PONT-POSTO       NOT NUMERIC)
               MOVE  1202              TO GFCTY1-COD-MSG-ERRO
               MOVE '1202'             TO GFCTG2-COD-MSG

               PERFORM 0500-FORMATAR-COD-ERRO
           END-IF.

           IF (GFCTY0-PONT-CNPJ-CPF    NOT NUMERIC) OR
              (GFCTY0-PONT-FILIAL      NOT NUMERIC) OR
              (GFCTY0-PONT-CONTROLE    NOT NUMERIC)
               MOVE  1203              TO GFCTY1-COD-MSG-ERRO
               MOVE '1203'             TO GFCTG2-COD-MSG

               PERFORM 0500-FORMATAR-COD-ERRO
           END-IF.

           IF (GFCTY0-PONT-COD-MUNIC   NOT NUMERIC)
               MOVE  1204              TO GFCTY1-COD-MSG-ERRO
               MOVE '1204'             TO GFCTG2-COD-MSG

               PERFORM 0500-FORMATAR-COD-ERRO
           END-IF.

           IF (GFCTY0-PONT-UF          EQUAL LOW-VALUES)
               MOVE  1205              TO GFCTY1-COD-MSG-ERRO
               MOVE '1205'             TO GFCTG2-COD-MSG

               PERFORM 0500-FORMATAR-COD-ERRO
           END-IF.


           IF (GFCTY0-PONT-DATA-SUSP   EQUAL LOW-VALUES)
               MOVE  1187              TO GFCTY1-COD-MSG-ERRO
               MOVE '1187'             TO GFCTG2-COD-MSG

               PERFORM 0500-FORMATAR-COD-ERRO
           END-IF.

           IF (GFCTY0-QTDE-OCOR        NOT NUMERIC)
               MOVE  1131              TO GFCTY1-COD-MSG-ERRO
TESTE          MOVE '1131'             TO GFCTG2-COD-MSG

               PERFORM 0500-FORMATAR-COD-ERRO
           END-IF.

           IF (GFCTY0-QTDE-TOT-REG     NOT NUMERIC)
               MOVE  1132              TO GFCTY1-COD-MSG-ERRO
               MOVE '1132'             TO GFCTG2-COD-MSG

               PERFORM 0500-FORMATAR-COD-ERRO
           END-IF.

           IF (GFCTY0-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES)
               MOVE  1133              TO GFCTY1-COD-MSG-ERRO
               MOVE '1133'             TO GFCTG2-COD-MSG

               PERFORM 0500-FORMATAR-COD-ERRO
           END-IF.

           IF (GFCTY0-FIM              NOT EQUAL 'S' AND 'N')
               MOVE  1134              TO GFCTY1-COD-MSG-ERRO
TESTE          MOVE '1134'             TO GFCTG2-COD-MSG

               PERFORM 0500-FORMATAR-COD-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * FORMATAR CODIGO DE ERRO                                        *
      *----------------------------------------------------------------*
       0500-FORMATAR-COD-ERRO          SECTION.
      *----------------------------------------------------------------*

           MOVE 'S'                    TO GFCTY1-FIM.
           MOVE 1                      TO GFCTY1-ERRO.
TESTE *---                                GFCTY1-COD-MSG-ERRO.
TESTE *--- MOVE ZEROS                  TO GFCTY1-COD-SQL-ERRO.

           PERFORM 0600-OBTER-DESCRICAO-MSG.

           PERFORM 9000-FINALIZAR.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * OBTEM A DESCRICAO DA MENSAGEM                                  *
      *----------------------------------------------------------------*
       0600-OBTER-DESCRICAO-MSG        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE                  GFCTG3-SAIDA.

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE GFCTY0-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTY0-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTY0-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'N'                    TO GFCTG2-FIM.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE ZEROS              TO RETURN-CODE
               MOVE 9                  TO GFCTY1-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0940'         TO GFCT0M-TRANSACAO

               PERFORM 9000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 'GFCT0940'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTY1-ERRO

               IF  GFCT0M-TIPO-ACESSO  EQUAL 'DB2'
                   MOVE GFCTG3-COD-SQL-ERRO
                                       TO GFCTY1-COD-SQL-ERRO
                   MOVE GFCTG3-COD-MSG-ERRO
                                       TO GFCTY1-COD-MSG-ERRO
                   MOVE GFCTG3-DESC-MSG-ERRO
                                       TO GFCTY1-DESC-MSG-ERRO
               ELSE
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
                   MOVE WRK-MSG05      TO GFCT0M-TEXTO
               END-IF

               PERFORM 9000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE 'S'                TO GFCTY1-FIM

               MOVE  1                 TO GFCTY1-ERRO

               MOVE  GFCTG3-COD-SQL-ERRO
                                       TO GFCTY1-COD-SQL-ERRO
               MOVE  GFCTG3-COD-MSG-ERRO
                                       TO GFCTY1-COD-MSG-ERRO
               MOVE  GFCTG3-DESC-MSG-ERRO
                                       TO GFCTY1-DESC-MSG-ERRO

               PERFORM 9000-FINALIZAR
           END-IF.

           MOVE GFCTG3-DESC-MSG        TO GFCTY1-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * SELECAO NA TABELA GFCTB0A1                                     *
      *----------------------------------------------------------------*
       0900-OBTER-DATA-PROC            SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC  OF GFCTB0A1.

           EXEC SQL
               SELECT  DPROCM_ATUAL
               INTO   :GFCTB0A1.DPROCM-ATUAL
               FROM    DB2PRD.PARM_DATA_PROCM
               WHERE   CSIST_PRINC     = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0010'             TO GFCT0M-LOCAL

               MOVE  0012              TO GFCTY1-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0A1'         TO WRK-NOME-TAB
               MOVE SQLCODE            TO WRK-SQLCODE

               PERFORM 1000-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * OBTENCAO DA MENSAGEM DB2                                       *
      *----------------------------------------------------------------*
       1000-FORMATAR-ERRO-DB2          SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT0940'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE  WRK-SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTY1-COD-SQL-ERRO.
           MOVE  SQLCA                 TO GFCT0M-SQLCA-AREA.
           MOVE  9                     TO GFCTY1-ERRO.

           PERFORM 0600-OBTER-DESCRICAO-MSG.

           STRING  GFCTY1-DESC-MSG-ERRO
                   WRK-NOME-TABELA

           DELIMITED BY '   '          INTO GFCTY1-DESC-MSG-ERRO.

           PERFORM 9000-FINALIZAR.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * PROCEDIMENTOS INICIAIS PARA PROCESSAMENTO DO CURSOR            *
      *----------------------------------------------------------------*
       1100-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE  GFCTY0-AGPTO          TO CAGPTO-CTA  OF  GFCTB0E7.

           IF  GFCTY0-TARIFA           EQUAL ZEROS
               MOVE ZEROS              TO WRK-TARIFA-INI
TESTE          MOVE 99999              TO WRK-TARIFA-FIM
           ELSE
               MOVE GFCTY0-TARIFA
                                       TO WRK-TARIFA-INI
                                          WRK-TARIFA-FIM
           END-IF.

           IF  GFCTY0-PONT-CSEQ-AGPTO  EQUAL ZEROS
               MOVE ZEROS              TO WRK-CSEQ-AGPTO
           ELSE
               MOVE GFCTY0-PONT-CSEQ-AGPTO
                                       TO WRK-CSEQ-AGPTO
           END-IF.

           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO WRK-DPROCM-ATUAL.

           PERFORM 1400-ABRIR-CURSOR-1

           PERFORM 1500-LER-CURSOR-1   VARYING IND-1 FROM 0 BY 1
                                       UNTIL IND-1 GREATER
                                       GFCTY0-QTDE-OCOR OR
                                       WRK-FIM-CURSOR EQUAL 'S'.

           MOVE  ZEROS                 TO  IND-1.

           PERFORM 1600-PROCESSAR-CURSOR-1
                                       VARYING IND-1 FROM 1 BY 1
                                       UNTIL IND-1 GREATER 7 OR
                                       WRK-FIM-CURSOR EQUAL 'S'.

           IF  WRK-FIM-CURSOR          NOT EQUAL 'S'
               MOVE 'N'                TO GFCTY1-FIM

               IF  IND-1               GREATER 7
                   MOVE CSEQ-AGPTO-CTA OF GFCTB0E7
                                       TO GFCTY0-PONT-CSEQ-AGPTO

                   MOVE CSERVC-TARIF   OF GFCTB0E7
                                       TO GFCTY0-PONT-TARIFA
               END-IF
           END-IF.

           PERFORM 1800-FECHAR-CURSOR-1.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * VERIFICAR QUANTIDADE DE OCORRENCIAS NO CURSOR 1                *
      *----------------------------------------------------------------*
       1300-COUNT-CURSOR-1             SECTION.
      *----------------------------------------------------------------*
           EXEC SQL
               SELECT  COUNT (*)
               INTO   :WRK-COUNT
               FROM  DB2PRD.SUSP_TARIF_AGPTO
              WHERE  CAGPTO_CTA         = :GFCTB0E7.CAGPTO-CTA
                AND (CSERVC_TARIF      >= :WRK-TARIFA-INI
                AND  CSERVC_TARIF      <= :WRK-TARIFA-FIM)
                AND  CSEQ_AGPTO_CTA    >= :WRK-CSEQ-AGPTO
                AND  DFIM_SUSP_COBR    <= :WRK-DPROCM-ATUAL
                AND  CINDCD_LIBRC_COBR IN ('N', 'C')
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'SUSP_GRP_CLI'     TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL

               MOVE  0012              TO GFCTY1-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0E3'         TO WRK-NOME-TAB
               MOVE SQLCODE            TO WRK-SQLCODE

               PERFORM 1000-FORMATAR-ERRO-DB2
           END-IF.

           IF  WRK-COUNT               EQUAL ZEROS
               MOVE 1206               TO GFCTY1-COD-MSG-ERRO
               MOVE '1206'             TO GFCTG2-COD-MSG

               PERFORM 0500-FORMATAR-COD-ERRO
           ELSE
               IF  WRK-COUNT           GREATER 150
                   MOVE  0633          TO GFCTY1-COD-MSG-ERRO
                   MOVE '0633'         TO GFCTG2-COD-MSG

                   PERFORM 0500-FORMATAR-COD-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ABERTURA DO CURSOR 1                                           *
      *----------------------------------------------------------------*
       1400-ABRIR-CURSOR-1             SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN CSR01-GFCTB0E7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'SUSP_TARIF_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL

               MOVE  0005              TO GFCTY1-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0E3'         TO WRK-NOME-TAB
               MOVE SQLCODE            TO WRK-SQLCODE

               PERFORM 1000-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * LEITURA DO CURSOR 1                                            *
      *----------------------------------------------------------------*
       1500-LER-CURSOR-1               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              FETCH  CSR01-GFCTB0E7
               INTO :GFCTB0E7.CSERVC-TARIF,
                    :GFCTB0E7.CSEQ-AGPTO-CTA,
TESTE               :GFCTB0E7.DINIC-SUSP-COBR,
                    :GFCTB0E7.DFIM-SUSP-COBR,
TESTE               :GFCTB0E7.CINDCD-AGPTO-TOT
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE 'SUSP_TARIF_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL

               MOVE  0006              TO GFCTY1-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0E3'         TO WRK-NOME-TAB
               MOVE SQLCODE            TO WRK-SQLCODE

               PERFORM 1800-FECHAR-CURSOR-1

               PERFORM 1000-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR
                                          GFCTY1-FIM
           END-IF.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * PROCESSAR CURSOR 1                                             *
      *----------------------------------------------------------------*
       1600-PROCESSAR-CURSOR-1         SECTION.
      *----------------------------------------------------------------*

           ADD +119                    TO GFCTY1-LL.
           ADD  1                      TO GFCTY1-QTDE-OCOR
                                          GFCTY1-QTDE-TOT-REG.

           MOVE CSEQ-AGPTO-CTA         OF GFCTB0E7
                                       TO GFCTY1-CSEQ-AGPTO (IND-1).

           MOVE CSERVC-TARIF           OF GFCTB0E7
                                       TO GFCTY1-COD-TARIFA (IND-1)
                                          CSERVC-TARIF OF GFCTB0D8.

           PERFORM 1700-PESQUISA-DESC-TARIFA.

           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO GFCTY1-DESC-TARIFA(IND-1).

           MOVE DFIM-SUSP-COBR         OF GFCTB0E7
                                       TO GFCTY1-DATA-FIM-SUSP(IND-1).

TESTE      MOVE DINIC-SUSP-COBR        OF GFCTB0E7
TESTE                                  TO GFCTY1-DATA-INIC-SUSP(IND-1).

           EVALUATE GFCTY0-AGPTO
               WHEN 4
                   PERFORM 2000-CONSULTAR-CNPJ-CPF
               WHEN 14
                   PERFORM 3000-CONSULTAR-AGENCIA
               WHEN 15
                   PERFORM 4000-CONSULTAR-PAB
               WHEN 18
                   PERFORM 5000-CONSULTAR-MUNICIPIO
               WHEN 19
                   PERFORM 6000-CONSULTAR-UF
           END-EVALUATE.

           PERFORM 1500-LER-CURSOR-1.

      *----------------------------------------------------------------*
       1600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * OBTER DESCRICAO DA TARIFA                                      *
      *----------------------------------------------------------------*
       1700-PESQUISA-DESC-TARIFA      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                     RSERVC_TARIF_REDZD
             INTO
                    :GFCTB0D8.RSERVC-TARIF-REDZD

             FROM   DB2PRD.SERVC_TARIF_PRINC
             WHERE
                   CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF

           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE 'SERVC_TARIF_PRINC '
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL

               MOVE  0012              TO GFCTY1-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0D8'         TO WRK-NOME-TAB
               MOVE SQLCODE            TO WRK-SQLCODE

               PERFORM 1000-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       1700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * FECHAMENTO DO CURSOR 1                                         *
      *----------------------------------------------------------------*
       1800-FECHAR-CURSOR-1            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR01-GFCTB0E7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'SUSP_TARIF_CTA  ' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0060'             TO GFCT0M-LOCAL

               MOVE  0011              TO GFCTY1-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0E7'         TO GFCT0M-NOME-TAB
               MOVE  SQLCODE           TO WRK-SQLCODE

               PERFORM 1000-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       1800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * CONSULTAR CNPJ CPF                                             *
      *----------------------------------------------------------------*
       2000-CONSULTAR-CNPJ-CPF         SECTION.
      *----------------------------------------------------------------*

           MOVE  CSERVC-TARIF    OF  GFCTB0E7
                                 TO  CSERVC-TARIF    OF  GFCTB0E3.

           MOVE  CAGPTO-CTA      OF  GFCTB0E7
                                 TO  CAGPTO-CTA      OF  GFCTB0E3.

           MOVE  DINIC-SUSP-COBR OF  GFCTB0E7
                                 TO  DINIC-SUSP-COBR OF  GFCTB0E3.

           MOVE  CSEQ-AGPTO-CTA  OF  GFCTB0E7
                                 TO  CSEQ-AGPTO-CTA  OF  GFCTB0E3.
           EXEC SQL
               SELECT
                     CCGC_CPF,
                     CFLIAL_CGC,
                     CCTRL_CPF_CGC,
ST25X6               CCGC_CPF_ST,
ST25X6               CFLIAL_CGC_ST,
ST25X6               CCTRL_CPF_CGC_ST
                INTO
                    :GFCTB0E3.CCGC-CPF,
                    :GFCTB0E3.CFLIAL-CGC,
                    :GFCTB0E3.CCTRL-CPF-CGC,
ST25X6              :GFCTB0E3.CCGC-CPF-ST,
ST25X6              :GFCTB0E3.CFLIAL-CGC-ST,
ST25X6              :GFCTB0E3.CCTRL-CPF-CGC-ST

                FROM   DB2PRD.SUSP_GRP_CLI

                WHERE  CSERVC_TARIF    = :GFCTB0E3.CSERVC-TARIF
                AND    CAGPTO_CTA      = :GFCTB0E3.CAGPTO-CTA
                AND    DINIC_SUSP_COBR = :GFCTB0E3.DINIC-SUSP-COBR
                AND    CSEQ_AGPTO_CTA  = :GFCTB0E3.CSEQ-AGPTO-CTA

           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE 'SERVC_TARIF_PRINC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL

               MOVE  0012              TO GFCTY1-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0D8'         TO WRK-NOME-TAB
               MOVE  SQLCODE           TO WRK-SQLCODE

               PERFORM 1000-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  SQLCODE           TO GFCTY1-COD-SQL-ERRO

               MOVE  0926              TO GFCTY1-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG

               PERFORM 0500-FORMATAR-COD-ERRO
           END-IF.

ST25X6*    MOVE   CCGC-CPF             OF  GFCTB0E3
ST25X6*                                TO  GFCTY1-CNPJ-CPF(IND-1).
ST25X6     MOVE   CCGC-CPF-ST          OF  GFCTB0E3
ST25X6                                 TO  GFCTY1-CNPJ-CPF(IND-1).

ST25X6*    MOVE   CFLIAL-CGC           OF  GFCTB0E3
ST25X6*                                TO  GFCTY1-FILIAL(IND-1).
ST25X6     MOVE   CFLIAL-CGC-ST        OF  GFCTB0E3
ST25X6                                 TO  GFCTY1-FILIAL(IND-1).

ST25X6*    MOVE   CCTRL-CPF-CGC        OF  GFCTB0E3
ST25X6*                                TO  GFCTY1-CONTROLE(IND-1).
ST25X6     MOVE   CCTRL-CPF-CGC-ST     OF  GFCTB0E3
ST25X6                                 TO  GFCTY1-CONTROLE(IND-1).

           IF  IND-1  NOT LESS  7
ST25X6*        MOVE   CCGC-CPF         OF  GFCTB0E3
ST25X6*                                TO  GFCTY1-PONT-CNPJ-CPF
ST25X6         MOVE   CCGC-CPF-ST      OF  GFCTB0E3
ST25X6                                 TO  GFCTY1-PONT-CNPJ-CPF

ST25X6*        MOVE   CFLIAL-CGC       OF  GFCTB0E3
ST25X6*                                TO  GFCTY1-PONT-FILIAL
ST25X6         MOVE   CFLIAL-CGC-ST    OF  GFCTB0E3
ST25X6                                 TO  GFCTY1-PONT-FILIAL

ST25X6*        MOVE   CCTRL-CPF-CGC    OF  GFCTB0E3
ST25X6*                                TO  GFCTY1-PONT-CONTROLE
ST25X6         MOVE   CCTRL-CPF-CGC-ST OF  GFCTB0E3
ST25X6                                 TO  GFCTY1-PONT-CONTROLE
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * CONSULTAR AGENCIA                                              *
      *----------------------------------------------------------------*
       3000-CONSULTAR-AGENCIA          SECTION.
      *----------------------------------------------------------------*

           MOVE  CSERVC-TARIF    OF  GFCTB0E7
                                 TO  CSERVC-TARIF    OF  GFCTB0E4.

           MOVE  CAGPTO-CTA      OF  GFCTB0E7
                                 TO  CAGPTO-CTA      OF  GFCTB0E4.

           MOVE  DINIC-SUSP-COBR OF  GFCTB0E7
                                 TO  DINIC-SUSP-COBR OF  GFCTB0E4.

           MOVE  CSEQ-AGPTO-CTA  OF  GFCTB0E7
                                 TO  CSEQ-AGPTO-CTA  OF  GFCTB0E4.

           EXEC SQL
              SELECT
                      CDEPDC
              INTO
                      :GFCTB0E4.CDEPDC

                FROM   DB2PRD. SUSP_GRP_DEPDC

                WHERE  CSERVC_TARIF    = :GFCTB0E4.CSERVC-TARIF
                AND    CAGPTO_CTA      = :GFCTB0E4.CAGPTO-CTA
                AND    DINIC_SUSP_COBR = :GFCTB0E4.DINIC-SUSP-COBR
                AND    CSEQ_AGPTO_CTA  = :GFCTB0E4.CSEQ-AGPTO-CTA

           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE 'SUSP_GRP_DEPDC  ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0090'             TO GFCT0M-LOCAL

               MOVE  0012              TO GFCTY1-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0E4'         TO WRK-NOME-TAB
               MOVE SQLCODE            TO WRK-SQLCODE

               PERFORM 1000-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  SQLCODE           TO GFCTY1-COD-SQL-ERRO

               MOVE  0926              TO GFCTY1-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG

               PERFORM 0500-FORMATAR-COD-ERRO
           END-IF.

           MOVE   CDEPDC               OF  GFCTB0E4
                                       TO  GFCTY1-AGENCIA(IND-1).

TESTE      IF  CINDCD-AGPTO-TOT OF GFCTB0E7
TESTE                                  EQUAL  'S'
TESTE          MOVE  'TODAS'           TO  GFCTY1-DESCRICAO(IND-1)
TESTE      ELSE
               PERFORM 3100-OBTER-NOME-AGENCIA
TESTE      END-IF.

           IF  IND-1  NOT LESS  7
               MOVE   CDEPDC           OF  GFCTB0E4
                                       TO  GFCTY1-PONT-AGENCIA
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * OBTER NOME DA AGENCIA                                          *
      *----------------------------------------------------------------*
       3100-OBTER-NOME-AGENCIA         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  COMU-PARM
                                       WRK-POOL7100
                                       WRK-SQLCA.

           MOVE 237                    TO COMU-BANCO.

           MOVE CDEPDC                 OF GFCTB0E4
                                       TO COMU-DEPDC.

           MOVE 'GFCT0940'             TO COMU-NOME-PGM.

           MOVE 'MESU9015'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             COMU-PARM
                                             WRK-POOL7100
                                             WRK-SQLCA.

           IF  COMU-COD-RETORNO        NOT EQUAL ZEROS
               MOVE  ZEROS             TO GFCTY1-COD-SQL-ERRO

               MOVE  0465              TO GFCTY1-COD-MSG-ERRO
               MOVE '0465'             TO GFCTG2-COD-MSG

               PERFORM 0500-FORMATAR-COD-ERRO
           END-IF.

           MOVE COMU-NOME-DEPDC (1:20) TO GFCTY1-DESCRICAO(IND-1).

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * CONSULTAR PAB                                                  *
      *----------------------------------------------------------------*
       4000-CONSULTAR-PAB              SECTION.
      *----------------------------------------------------------------*

           MOVE  CSERVC-TARIF    OF  GFCTB0E7
                                 TO  CSERVC-TARIF    OF  GFCTB0I1.

           MOVE  CAGPTO-CTA      OF  GFCTB0E7
                                 TO  CAGPTO-CTA      OF  GFCTB0I1.

           MOVE  DINIC-SUSP-COBR OF  GFCTB0E7
                                 TO  DINIC-SUSP-COBR OF  GFCTB0I1.

           MOVE  CSEQ-AGPTO-CTA  OF  GFCTB0E7
                                 TO  CSEQ-AGPTO-CTA  OF  GFCTB0I1.

           EXEC SQL
               SELECT
                      CDEPDC,
                      CPOSTO_SERVC
               INTO
                      :GFCTB0I1.CDEPDC,
                      :GFCTB0I1.CPOSTO-SERVC

                FROM   DB2PRD.TSUSP_GRP_PAB

                WHERE  CSERVC_TARIF    = :GFCTB0I1.CSERVC-TARIF
                AND    CAGPTO_CTA      = :GFCTB0I1.CAGPTO-CTA
                AND    DINIC_SUSP_COBR = :GFCTB0I1.DINIC-SUSP-COBR
                AND    CSEQ_AGPTO_CTA  = :GFCTB0I1.CSEQ-AGPTO-CTA

           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE 'TSUSP_GRP_PAB'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0130'             TO GFCT0M-LOCAL

               MOVE  0012              TO GFCTY1-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0I1'         TO WRK-NOME-TAB
               MOVE SQLCODE            TO WRK-SQLCODE

               PERFORM 1000-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  SQLCODE           TO GFCTY1-COD-SQL-ERRO

               MOVE  0926              TO GFCTY1-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG

               PERFORM 0500-FORMATAR-COD-ERRO
           END-IF.

           MOVE CDEPDC                 OF GFCTB0I1
                                       TO GFCTY1-AGENCIA(IND-1).

           MOVE CPOSTO-SERVC           OF GFCTB0I1
                                       TO GFCTY1-POSTO(IND-1).

           PERFORM 4100-OBTER-NOME-PAB.

           IF  IND-1  NOT LESS  7
                MOVE CDEPDC            OF GFCTB0I1
                                       TO GFCTY1-PONT-AGENCIA
                MOVE CPOSTO-SERVC      OF GFCTB0I1
                                       TO GFCTY1-PONT-POSTO
           END-IF.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * OBTER DESCRICAO PAB                                            *
      *----------------------------------------------------------------*
       4100-OBTER-NOME-PAB             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  COMU-DADOS-9018.

           MOVE 'VRS001'               TO COMU-VERSAO.
           MOVE 237                    TO COMU-COD-EMPR.
           MOVE CDEPDC                 OF GFCTB0I1
                                       TO COMU-COD-DEPDC.
           MOVE CPOSTO-SERVC           OF GFCTB0I1
                                       TO COMU-COD-POSTO.
           MOVE SPACES                 TO COMU-CORR-BANCARIO.

           MOVE 'MESU9018'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             COMU-DADOS-9018.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE  ZEROS             TO GFCTY1-COD-SQL-ERRO

               EVALUATE RETURN-CODE
                   WHEN 4
                        MOVE  0518     TO GFCTY1-COD-MSG-ERRO
                        MOVE '0518'    TO GFCTG2-COD-MSG
                   WHEN 8
                        MOVE  0155     TO GFCTY1-COD-MSG-ERRO
                        MOVE '0155'    TO GFCTG2-COD-MSG
                   WHEN 12
                        MOVE  0520     TO GFCTY1-COD-MSG-ERRO
                        MOVE '0520'    TO GFCTG2-COD-MSG
               END-EVALUATE

               MOVE ZEROS              TO RETURN-CODE

               PERFORM 0500-FORMATAR-COD-ERRO
           ELSE
               IF  COMU-COD-TIPO-POSTO EQUAL 9
                   MOVE  ZEROS         TO GFCTY1-COD-SQL-ERRO

                   MOVE  0594          TO GFCTY1-COD-MSG-ERRO
                   MOVE '0594'         TO GFCTG2-COD-MSG

                   PERFORM 0500-FORMATAR-COD-ERRO
               END-IF
           END-IF.

           MOVE COMU-NOME-POSTO        TO GFCTY1-DESCRICAO (IND-1).

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * CONSULTAR MUNICIPIO                                            *
      *----------------------------------------------------------------*
       5000-CONSULTAR-MUNICIPIO        SECTION.
      *----------------------------------------------------------------*

           MOVE  CSERVC-TARIF    OF  GFCTB0E7
                                 TO  CSERVC-TARIF    OF  GFCTB0E5.

           MOVE  CAGPTO-CTA      OF  GFCTB0E7
                                 TO  CAGPTO-CTA      OF  GFCTB0E5.

           MOVE  DINIC-SUSP-COBR OF  GFCTB0E7
                                 TO  DINIC-SUSP-COBR OF  GFCTB0E5.

           MOVE  CSEQ-AGPTO-CTA  OF  GFCTB0E7
                                 TO  CSEQ-AGPTO-CTA  OF  GFCTB0E5.

           EXEC SQL
               SELECT
                       CMUN_IBGE
               INTO
                      :GFCTB0E5.CMUN-IBGE

               FROM   DB2PRD.SUSP_GRP_MUN

                WHERE  CSERVC_TARIF    = :GFCTB0E5.CSERVC-TARIF
                AND    CAGPTO_CTA      = :GFCTB0E5.CAGPTO-CTA
                AND    DINIC_SUSP_COBR = :GFCTB0E5.DINIC-SUSP-COBR
                AND    CSEQ_AGPTO_CTA  = :GFCTB0E5.CSEQ-AGPTO-CTA

           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE 'SUSP_GRP_MUN'     TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0170'             TO GFCT0M-LOCAL

               MOVE  0012              TO GFCTY1-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0E5'         TO WRK-NOME-TAB
               MOVE  SQLCODE           TO WRK-SQLCODE

               PERFORM 1000-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  SQLCODE           TO GFCTY1-COD-SQL-ERRO

               MOVE  0926              TO GFCTY1-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG

               PERFORM 0500-FORMATAR-COD-ERRO
           END-IF.

           MOVE CMUN-IBGE              OF GFCTB0E5
                                       TO GFCTY1-COD-MUNIC(IND-1).

           PERFORM  5100-OBTER-NOME-MUNICIPIO.

           IF  IND-1                   NOT LESS  7
               MOVE CMUN-IBGE          OF GFCTB0E5
                                       TO GFCTY1-PONT-COD-MUNIC
           END-IF.

      *----------------------------------------------------------------*
       5000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * OBTER NOME DO MUNICIPIO                                        *
      *----------------------------------------------------------------*
       5100-OBTER-NOME-MUNICIPIO       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE AREA-RURC88.

           MOVE CMUN-IBGE              OF GFCTB0E5
                                       TO COD-MUNIC-RURC88.

           MOVE 'RURC9020'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             AREA-RURC88.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE  ZEROS             TO GFCTY1-COD-SQL-ERRO

               MOVE  0732              TO GFCTY1-COD-MSG-ERRO
               MOVE '0732'             TO GFCTG2-COD-MSG

               PERFORM 0500-FORMATAR-COD-ERRO
           END-IF.

           IF  COD-RETORNO-RURC88      NOT EQUAL ZEROS
               MOVE  ZEROS             TO GFCTY1-COD-SQL-ERRO

               EVALUATE COD-RETORNO-RURC88
                   WHEN 99
                        MOVE SQLCODE-RURC88
                                       TO GFCTY1-COD-SQL-ERRO
                        MOVE 0734      TO GFCTY1-COD-MSG-ERRO
                        MOVE '0734'    TO GFCTG2-COD-MSG
                   WHEN 20
                        MOVE 0733      TO GFCTY1-COD-MSG-ERRO
                        MOVE '0733'    TO GFCTG2-COD-MSG
                   WHEN OTHER
                        MOVE 0675      TO GFCTY1-COD-MSG-ERRO
                        MOVE '0675'    TO GFCTG2-COD-MSG
               END-EVALUATE

               PERFORM 0500-FORMATAR-COD-ERRO
           END-IF.

           MOVE NOME-MUNIC-RURC88 (1:20)
                                       TO GFCTY1-DESCRICAO (IND-1).

      *----------------------------------------------------------------*
       5100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * CONSULTAR UF                                                   *
      *----------------------------------------------------------------*
       6000-CONSULTAR-UF               SECTION.
      *----------------------------------------------------------------*

           MOVE  CSERVC-TARIF    OF  GFCTB0E7
                                 TO  CSERVC-TARIF    OF  GFCTB0E6

           MOVE  CAGPTO-CTA      OF  GFCTB0E7
                                 TO  CAGPTO-CTA      OF  GFCTB0E6

           MOVE  DINIC-SUSP-COBR OF  GFCTB0E7
                                 TO  DINIC-SUSP-COBR OF  GFCTB0E6

           MOVE  CSEQ-AGPTO-CTA  OF  GFCTB0E7
                                 TO  CSEQ-AGPTO-CTA  OF  GFCTB0E6

           EXEC SQL
               SELECT
                      CSGL_UF
               INTO
                      :GFCTB0E6.CSGL-UF
               FROM
                      DB2PRD.SUSP_GRP_UF

                WHERE  CSERVC_TARIF    = :GFCTB0E6.CSERVC-TARIF
                AND    CAGPTO_CTA      = :GFCTB0E6.CAGPTO-CTA
                AND    DINIC_SUSP_COBR = :GFCTB0E6.DINIC-SUSP-COBR
                AND    CSEQ_AGPTO_CTA  = :GFCTB0E6.CSEQ-AGPTO-CTA

           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE 'SUSP_GRP_UF'      TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0210'             TO GFCT0M-LOCAL

               MOVE 'GFCTB0E6'         TO WRK-NOME-TAB
               MOVE  SQLCODE           TO WRK-SQLCODE

               MOVE  0012              TO GFCTY1-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG

               PERFORM 1000-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  SQLCODE           TO GFCTY1-COD-SQL-ERRO

               MOVE  0926              TO GFCTY1-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG

               PERFORM 0500-FORMATAR-COD-ERRO
           END-IF.

           MOVE CSGL-UF                OF  GFCTB0E6
                                       TO  GFCTY1-UF(IND-1).

           PERFORM 6100-OBTER-NOME-UF.

           IF  IND-1                   NOT LESS  7
               MOVE CSGL-UF            OF GFCTB0E6
                                       TO GFCTY1-PONT-UF
           END-IF.

      *----------------------------------------------------------------*
       6000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * OBTER NOME UF                                                  *
      *----------------------------------------------------------------*
       6100-OBTER-NOME-UF              SECTION.
      *----------------------------------------------------------------*

           MOVE CSGL-UF                OF GFCTB0E6
                                       TO CSGL-UF  OF DCITV002.

           MOVE DCITV002               TO WRK-AREA-DCLGEN-DCITV002.

           MOVE 'DCIT8000'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-DCIT-UNIDADE-FEDERATIVA
                                             WRK-POOL7100
                                             WRK-SQLCA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE  ZEROS             TO GFCTY1-COD-SQL-ERRO

               MOVE  0735              TO GFCTY1-COD-MSG-ERRO
               MOVE '0735'             TO GFCTG2-COD-MSG

               PERFORM 0500-FORMATAR-COD-ERRO
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 04
               MOVE 'V01_UF'           TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0220'             TO GFCT0M-LOCAL

               MOVE 'DCITV002'         TO WRK-NOME-TAB
               MOVE  WRK-SQLCA         TO SQLCA
               MOVE  SQLCODE           TO WRK-SQLCODE

               MOVE  0012              TO GFCTY1-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG

               PERFORM 1000-FORMATAR-ERRO-DB2
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO NOT EQUAL ZEROS
               MOVE  ZEROS             TO GFCTY1-COD-SQL-ERRO

               EVALUATE WRK-DCIT-CODIGO-RETORNO
                   WHEN 1
                        MOVE  0736     TO GFCTY1-COD-MSG-ERRO
                        MOVE '0736'    TO GFCTG2-COD-MSG
                   WHEN 2
                        MOVE  0737     TO GFCTY1-COD-MSG-ERRO
                        MOVE '0737'    TO GFCTG2-COD-MSG
                   WHEN 3
                        MOVE  0738     TO GFCTY1-COD-MSG-ERRO
                        MOVE '0738'    TO GFCTG2-COD-MSG
               END-EVALUATE

               PERFORM 0500-FORMATAR-COD-ERRO
           END-IF.

           MOVE WRK-AREA-DCLGEN-DCITV002
                                       TO DCITV002.
           MOVE IUF OF DCITV002(1:20)  TO GFCTY1-DESCRICAO(IND-1).

      *----------------------------------------------------------------*
       6100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * RETORNAR AO PROGRAMA CHAMADOR                                  *
      *----------------------------------------------------------------*
       9000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------

