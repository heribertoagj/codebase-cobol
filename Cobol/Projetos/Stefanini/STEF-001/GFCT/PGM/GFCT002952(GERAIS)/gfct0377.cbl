      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0377.
       AUTHOR. FABIO WILSON NAPOLI DE LUCA.
      *================================================================*
      *                   C P M   S I S T E M A S                      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT0377                                     *
      *    PROGRAMADOR  : FABIO WILSON NAPOLI DE LUCA - CPM/FPOLIS     *
      *    ANALISTA CPM : SILVANA F. DE SOUZA MACHADO - CPM/FPOLIS     *
      *    ANALISTA     : RENATO TAMANAHA - PROCWORK  - GRUPO 50       *
      *    DATA         : 26/08/2005                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      CONSULTA DE ESTORNO DE COBRANCA AGENCIAS/CONTAS.          *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *     DB2                                                        *
      *      TABLE                                 INCLUDE/BOOK        *
      *      DB2PRD.PDIDO_ESTRN_CTA                  GFCTB0A5          *
      *      DB2PRD.MOTVO_JUSTF_OPER                 GFCTB089          *
      *      DB2PRD.TPO_OPER_MOTVO                   GFCTB0G0          *
      *      DB2PRD.SERVC_TARIF_PRINC                GFCTB0D8          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5000 - OBTER DESCRICAO DE MENSAGENS                   *
      *      GFCT5508 - NOME DO CORRENTISTA                            *
      *      POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                 *
RIC   *      SARH8000 - PERMITIR AOS SISTEMAS LEGADOS ACESSO AS        *
RIC   *      INFORMACOES DE FUNCIONARIOS NO REPOSITORIO DE DADOS SAPHR.*
      *                                                                *
      *    NAVEGACAO:                                                  *
      *      CHAMADO POR: GFCT7383                                     *
      *                                                                *
      *================================================================*
      *                                                                *
      *    ALTERACAO :  PROCWORK - PROPOSTA 07-2287                    *
      *                                                                *
      *    ANALISTA     : MARCUS VINCIUS - PROCWORK  - GRUPO 50        *
      *    DATA         : 09/2007                                      *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      RETIRADA DE APRESENTACAO DOS DADOS DE EVENTO PARA TIPO    *
      *    DE SOLICITACAO  2 - SEM REGISTRO                            *
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
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT0377 *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA DE VARIAVEIS       *'.
      *----------------------------------------------------------------*

       01  WRK-MODULO                  PIC X(08)           VALUE SPACES.

RIC    01  WRK-DESC-FUNCIONARIO        PIC  X(040)        VALUE SPACES.
RIC
RIC    01  WRK-COD-FUNC                PIC 9(009)         VALUE ZEROS.
RIC    01  WRK-COD-FUNC-R              REDEFINES WRK-COD-FUNC.
RIC        05  FILLER                  PIC  9(002).
RIC        05  WRK-COD-FUNC-X7         PIC  X(007).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*     AREA PARA MENSAGENS      *'.
      *----------------------------------------------------------------*

       01  WRK-MSG05.
           03  FILLER                  PIC  X(26)          VALUE
               'ERRO NA CHAMADA AO MODULO'.
           03  WRK-MODULO-MSG          PIC  X(08)          VALUE SPACES.
           03  FILLER                  PIC  X(41)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA DE MODULO 5000     *'.
      *----------------------------------------------------------------*

       01  WRK-5000-AREA.
           03  WRK-5000-AREA-ENVIA.
               05  WRK-5000-AMBIENTE   PIC X(01)           VALUE SPACES.
               05  WRK-5000-COD-MSG    PIC 9(04)           VALUE ZEROS.
           03  WRK-5000-AREA-RECEBE.
               05  WRK-5000-COD-RETORNO
                                       PIC 9(02)           VALUE ZEROS.
               05  WRK-5000-COD-SQL-ERRO
                                       PIC 9(03)           VALUE ZEROS.
               05  WRK-5000-DESC-MSG   PIC X(70)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*      AREA DO MODULO 5508     *'.
      *----------------------------------------------------------------*

       01  WRK-5508-AREA-ENTRADA.
           03  WRK-5508-AMBIENTE       PIC  X(01)          VALUE SPACES.
           03  WRK-5508-CJUNC-DEPDC    PIC  9(05)          VALUE ZEROS.
           03  WRK-5508-CCTA-CLI       PIC  9(07)          VALUE ZEROS.

       01  WRK-5508-AREA-SAIDA.
           03  WRK-5508-COD-RETORNO    PIC  9(02)          VALUE ZEROS.
      *        00 - PROCESSAMENTO OK
      *        01 - AMBIENTE DIFERENTE DE 'B' E 'O'
      *        02 - REGISTRO NAO ENCONTRADO
      *        99 - ERRO DB2
           03  WRK-5508-COD-SQL-ERRO   PIC  9(03)          VALUE ZEROS.
           03  WRK-5508-DESC-MSG       PIC  X(70)          VALUE SPACES.
           03  WRK-5508-DADOS-RETORNO.
               05  WRK-5508-IPSSOA-COPLT
                                       PIC  X(70)          VALUE SPACES.

       01  WRK-IO-PCB.
           03  WRK-IO-TERM             PIC  X(08)          VALUE SPACES.
           03  FILLER                  PIC  X(02)          VALUE SPACES.
           03  WRK-IO-STA              PIC  X(02)          VALUE SPACES.
           03  FILLER                  PIC  X(12)          VALUE SPACES.
           03  WRK-IO-MODNAME          PIC  X(08)          VALUE SPACES.

       01  WRK-ALT-PCB.
           03  WRK-ALT-PCBNAME         PIC  X(08)          VALUE SPACES.
           03  FILLER                  PIC  X(02)          VALUE SPACES.
           03  WRK-ALT-STATUS          PIC  X(02)          VALUE SPACES.
           03  FILLER                  PIC  X(12)          VALUE SPACES.
           03  WRK-ALT-MODNAME         PIC  X(08)          VALUE SPACES.

RIC   *----------------------------------------------------------------*
RIC    01  FILLER                      PIC  X(050)         VALUE
RIC        '*** AREA MODULO SARH8000 ***'.
RIC   *----------------------------------------------------------------*
RIC
RIC    01  WRK-ENTRADA.
RIC        03  WRK-PESQUISA-ENT        PIC  9(009)         VALUE ZEROS.
RIC        03  WRK-TAMANHO-ENT         PIC  9(004)         VALUE ZEROS.
RIC        03  WRK-SOLICITA-ENT.
RIC            05  FILLER              OCCURS 255 TIMES.
RIC                07  WRK-CAMPO-ENT   PIC  9(003)         VALUE ZEROS.
RIC
RIC    01  WRK-SAIDA.
RIC        03  WRK-COD-RETORNO-SAI     PIC  9(002)         VALUE ZEROS.
RIC        03  WRK-TAMANHO-SAI         PIC  9(004)         VALUE ZEROS.
RIC        03  WRK-REGISTRO-SAI.
RIC            05  FILLER              OCCURS 0 TO 1000 TIMES
RIC                DEPENDING ON WRK-TAMANHO-SAI.
RIC                07  WRK-CONTADOR-SAI PIC  X(001)        VALUE SPACES.
RIC
RIC    01  WRK-ERRO-AREA               PIC  X(107)         VALUE SPACES.
RIC    01  WRK-SQLCA                   PIC  X(136)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA DE TABELAS DB2     *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A5
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB089
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0G0
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0D8
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    FIM DA WORKING GFCT0377  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY I#GFCTF1.
       COPY I#GFCTF2.
       COPY I#GFCT0M.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTF1-ENTRADA
                                                      GFCTF2-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
       00000-ROTINA-PRINCIPAL          SECTION.
      *----------------------------------------------------------------*

           PERFORM 10000-INICIALIZAR-SAIDA.

           PERFORM 20000-CONSISTIR-DADOS.

           PERFORM 30000-PROCESSAR-PROGRAMA.

           PERFORM 40000-FINALIZAR.

      *----------------------------------------------------------------*
       00000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       10000-INICIALIZAR-SAIDA         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTF2-SAIDA.

           INITIALIZE                  GFCTF2-SAIDA
                                       GFCT0M-AREA-ERROS
                                       SQLCA.

           MOVE 1010                   TO GFCTF2-LL.
           MOVE GFCTF1-ZZ              TO GFCTF2-ZZ.
           MOVE GFCTF1-TRANSACAO       TO GFCTF2-TRANSACAO.
           MOVE GFCTF1-FUNCAO          TO GFCTF2-FUNCAO.
           MOVE GFCTF1-FUNC-BDSCO      TO GFCTF2-FUNC-BDSCO.
           MOVE ZEROS                  TO GFCTF2-ERRO
                                          GFCTF2-COD-SQL-ERRO
                                          GFCTF2-COD-MSG-ERRO
                                          WRK-5000-COD-MSG.

      *----------------------------------------------------------------*
       10000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       20000-CONSISTIR-DADOS           SECTION.
      *----------------------------------------------------------------*

           IF (GFCTF1-TRANSACAO        EQUAL SPACES) OR
              (GFCTF1-FUNCAO           EQUAL SPACES) OR
              (GFCTF1-FUNC-BDSCO       EQUAL SPACES) OR
              (GFCTF1-AGENCIA       IS NOT NUMERIC)  OR
              (GFCTF1-AGENCIA          EQUAL ZEROS)  OR
              (GFCTF1-CONTA         IS NOT NUMERIC)  OR
              (GFCTF1-CONTA            EQUAL ZEROS)  OR
              (GFCTF1-DATA-DEBITO      EQUAL SPACES)
               MOVE 1                  TO GFCTF2-ERRO
                                          GFCTF2-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE ZEROS              TO GFCTF2-COD-SQL-ERRO
               PERFORM 21000-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO GFCTF2-DESC-MSG-ERRO
               PERFORM 40000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       20000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21000-OBTER-DESC-MENSAGEM       SECTION.
      *----------------------------------------------------------------*

           MOVE 'O'                    TO WRK-5000-AMBIENTE.
           MOVE 'GFCT5000'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5000-AREA-ENVIA
                                             WRK-5000-AREA-RECEBE.

           IF  WRK-5000-COD-RETORNO    NOT EQUAL ZEROS
               IF  WRK-5000-COD-RETORNO
                                       EQUAL 01
                   MOVE 1              TO GFCTF2-ERRO
                   MOVE ZEROS          TO GFCTF2-COD-SQL-ERRO
                                          GFCTF2-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTF2-DESC-MSG-ERRO
               END-IF
               IF  WRK-5000-COD-RETORNO
                                       EQUAL 99
                   MOVE 1              TO GFCTF2-ERRO
                   MOVE WRK-5000-COD-SQL-ERRO
                                       TO GFCTF2-COD-SQL-ERRO
                   MOVE ZEROS          TO GFCTF2-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTF2-DESC-MSG-ERRO
               END-IF
               PERFORM 40000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       21000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       30000-PROCESSAR-PROGRAMA        SECTION.
      *----------------------------------------------------------------*

           PERFORM 31000-SELECIONAR-ESTORNO.

           MOVE RJUSTF-SOLTC-ESTRN-TEXT OF GFCTB0A5
                                       TO GFCTF2-DESC-COMENTARIO.

           INSPECT GFCTF2-DESC-COMENTARIO
                                       REPLACING ALL LOW-VALUE BY ' '.

           MOVE RJUSTF-PAREC-ESTRN-TEXT OF GFCTB0A5
                                       TO GFCTF2-DESC-JUSTIFICATIVA.

           INSPECT GFCTF2-DESC-JUSTIFICATIVA
                                       REPLACING ALL LOW-VALUE BY ' '.

           MOVE CTPO-OPER-MOTVO    OF GFCTB0A5
                                       TO GFCTF2-TIPO-OPERACAO.
           MOVE CMOTVO-JUSTF       OF GFCTB0A5
                                       TO GFCTF2-MTVO-ESTORNO.

           MOVE CTPO-SOLTC-ESTRN    OF GFCTB0A5
                                       TO GFCTF2-TIPO-REG.

           IF CTPO-SOLTC-ESTRN     OF GFCTB0A5
                                       EQUAL 2
P 2287*
P 2287*   ALTERACAO REFERENTE A PROPOSTA 2287
P 2287*
P 2287*       MOVE DOCOR-EVNTO     OF GFCTB0A5
P 2287*                                TO GFCTF2-DATA-EVENTO
P 2287*       MOVE VEVNTO-SEM-REG  OF GFCTB0A5
P 2287*                                TO GFCTF2-VALOR-EVENTO

P 2287        MOVE SPACES              TO GFCTF2-DATA-EVENTO
P 2287        MOVE VTARIF-CADTR-SIST  OF GFCTB0A5
P 2287                                 TO GFCTF2-VALOR-EVENTO
           END-IF.

           MOVE QEVNTO-AGRUP       OF GFCTB0A5
                                       TO GFCTF2-QTDE-TR.
           MOVE CSIT-SOLTC-ESTRN   OF GFCTB0A5
                                       TO GFCTF2-PARECER.
           MOVE CSIT-PROCM-ESTRN   OF GFCTB0A5
                                       TO GFCTF2-SITUACAO.

RIC        MOVE CFUNC-SOLTC-ESTRN      OF GFCTB0A5 TO  WRK-COD-FUNC
RIC        MOVE WRK-COD-FUNC-X7        TO  GFCTF2-COD-FUNC-SOL.

RIC        IF WRK-COD-FUNC-X7 NOT EQUAL ZEROS
RIC           PERFORM 36000-OBTER-DESC-FUNC
RIC           MOVE WRK-DESC-FUNCIONARIO TO  GFCTF2-DESC-FUNC-SOL
RIC        ELSE
RIC           MOVE SPACES               TO  GFCTF2-DESC-FUNC-SOL
RIC        END-IF.

RIC        MOVE CFUNC-PAREC-ESTRN      OF GFCTB0A5 TO  WRK-COD-FUNC
RIC        MOVE WRK-COD-FUNC-X7        TO  GFCTF2-FUNC-RESP-PAR.

RIC        IF WRK-COD-FUNC-X7 NOT EQUAL ZEROS
RIC           PERFORM 36000-OBTER-DESC-FUNC
RIC           MOVE WRK-DESC-FUNCIONARIO TO  GFCTF2-DESC-FUNC-PAR
RIC        ELSE
RIC           MOVE SPACES               TO  GFCTF2-DESC-FUNC-PAR
RIC        END-IF.

           PERFORM 32000-OBTER-NOME-CLIENTE.

           MOVE WRK-5508-IPSSOA-COPLT  TO GFCTF2-NOME-CLIENTE.

           MOVE GFCTF1-AGENCIA         TO GFCTF2-AGENCIA.
           MOVE GFCTF1-CONTA           TO GFCTF2-CONTA.
           MOVE DEFETV-DEB-MOVTO       OF GFCTB0A5
                                       TO GFCTF2-DATA-DEBITO.

           INSPECT GFCTF2-DATA-DEBITO  REPLACING ALL '.' BY '/'.

           MOVE DRECEB-SOLTC-ESTRN   OF GFCTB0A5
                                       TO GFCTF2-DATA-SOLICITACAO.

           INSPECT GFCTF2-DATA-SOLICITACAO
                                       REPLACING ALL '.' BY '/'.

           MOVE GFCTF1-TARIFA          TO GFCTF2-TARIFA.
           MOVE GFCTF1-VALOR           TO GFCTF2-VALOR.

           PERFORM 33000-SELECIONAR-TIPO-OPER.

           PERFORM 34000-SELECIONAR-MOVTO-ESTORNO.

           PERFORM 35000-SELECIONAR-TARIFA.

           MOVE RTPO-OPER-MOTVO    OF GFCTB0G0
                                       TO GFCTF2-DESC-TP-OPER.
           MOVE CIDTFD-UTILZ-ALCAD OF GFCTB0G0
                                       TO GFCTF2-COD-UTIL-OPER.

           MOVE RMOTVO-JUSTF       OF GFCTB089
                                       TO GFCTF2-DESC-MOTVO-ESTOR.
           MOVE RSERVC-TARIF-REDZD OF GFCTB0D8
                                       TO GFCTF2-DESC-TARIFA.

           MOVE 'S'                    TO GFCTF2-FIM.

      *----------------------------------------------------------------*
       30000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       31000-SELECIONAR-ESTORNO        SECTION.
      *----------------------------------------------------------------*

           MOVE 237                    TO CBCO-CTA-CLI     OF GFCTB0A5.
           MOVE GFCTF1-AGENCIA         TO CJUNC-DEPDC      OF GFCTB0A5.
           MOVE GFCTF1-CONTA           TO CCTA-CLI         OF GFCTB0A5.
           MOVE GFCTF1-TARIFA          TO CSERVC-TARIF     OF GFCTB0A5.
           MOVE GFCTF1-TIMESTAMP       TO HIDTFD-ESTRN-CTA OF GFCTB0A5.

           EXEC SQL
             SELECT CSERVC_TARIF,
                    CTPO_OPER_MOTVO,
                    RJUSTF_SOLTC_ESTRN,
                    RJUSTF_PAREC_ESTRN,
                    CTPO_SOLTC_ESTRN,
                    CSIT_SOLTC_ESTRN,
                    CSIT_PROCM_ESTRN,
RIC                 CFUNC_SOLTC_ESTRN,
RIC                 CFUNC_PAREC_ESTRN,
                    DRECEB_SOLTC_ESTRN,
                    DEFETV_DEB_MOVTO,
                    CMOTVO_JUSTF,
P 2287*             DOCOR_EVNTO,
P 2287*             VEVNTO_SEM_REG,
                    QEVNTO_AGRUP,
P 2287              VTARIF_CADTR_SIST
             INTO  :GFCTB0A5.CSERVC-TARIF,
                   :GFCTB0A5.CTPO-OPER-MOTVO,
                   :GFCTB0A5.RJUSTF-SOLTC-ESTRN,
                   :GFCTB0A5.RJUSTF-PAREC-ESTRN,
                   :GFCTB0A5.CTPO-SOLTC-ESTRN,
                   :GFCTB0A5.CSIT-SOLTC-ESTRN,
                   :GFCTB0A5.CSIT-PROCM-ESTRN,
RIC                :GFCTB0A5.CFUNC-SOLTC-ESTRN,
RIC                :GFCTB0A5.CFUNC-PAREC-ESTRN,
                   :GFCTB0A5.DRECEB-SOLTC-ESTRN,
                   :GFCTB0A5.DEFETV-DEB-MOVTO,
                   :GFCTB0A5.CMOTVO-JUSTF,
P 2287*            :GFCTB0A5.DOCOR-EVNTO,
P 2287*            :GFCTB0A5.VEVNTO-SEM-REG,
                   :GFCTB0A5.QEVNTO-AGRUP,
P 2287             :GFCTB0A5.VTARIF-CADTR-SIST
             FROM   DB2PRD.PDIDO_ESTRN_CTA
             WHERE  CBCO_CTA_CLI     = :GFCTB0A5.CBCO-CTA-CLI     AND
                    CJUNC_DEPDC      = :GFCTB0A5.CJUNC-DEPDC      AND
                    CCTA_CLI         = :GFCTB0A5.CCTA-CLI         AND
                    CSERVC_TARIF     = :GFCTB0A5.CSERVC-TARIF     AND
                    HIDTFD_ESTRN_CTA = :GFCTB0A5.HIDTFD-ESTRN-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'GFCT0377'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_ESTRN_CTA'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTF2-COD-SQL-ERRO
               MOVE '0001'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCTF2-ERRO
               MOVE 10                 TO GFCTF2-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 21000-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO GFCTF2-DESC-MSG-ERRO
               PERFORM 40000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       31000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       32000-OBTER-NOME-CLIENTE        SECTION.
      *----------------------------------------------------------------*

           MOVE 'O'                    TO WRK-5508-AMBIENTE.
           MOVE GFCTF1-AGENCIA         TO WRK-5508-CJUNC-DEPDC.
           MOVE GFCTF1-CONTA           TO WRK-5508-CCTA-CLI.
           MOVE 'GFCT5508'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5508-AREA-ENTRADA
                                             WRK-5508-AREA-SAIDA
                                             WRK-IO-PCB
                                             WRK-ALT-PCB.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTF2-ERRO

               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0377'         TO GFCT0M-PROGRAMA
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
                                          GFCTF2-DESC-MSG-ERRO

               PERFORM 40000-FINALIZAR
           END-IF.

           IF  WRK-5508-COD-RETORNO    EQUAL 02
               MOVE 1                  TO GFCTF2-ERRO

               MOVE 'S'                TO GFCTF2-FIM
               MOVE ZEROS              TO GFCTF2-COD-SQL-ERRO
               MOVE 0010               TO GFCTF2-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 21000-OBTER-DESC-MENSAGEM

               MOVE WRK-5000-DESC-MSG  TO GFCTF2-DESC-MSG-ERRO

               PERFORM 40000-FINALIZAR
           END-IF.

           IF  WRK-5508-COD-RETORNO    NOT EQUAL ZEROS
               MOVE 9                  TO GFCTF2-ERRO

               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0377'         TO GFCT0M-PROGRAMA
               MOVE WRK-5508-DESC-MSG  TO GFCT0M-TEXTO
                                          GFCTF2-DESC-MSG-ERRO

               PERFORM 40000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       32000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       33000-SELECIONAR-TIPO-OPER      SECTION.
      *----------------------------------------------------------------*

           MOVE CTPO-OPER-MOTVO OF GFCTB0A5
                                       TO CTPO-OPER-MOTVO OF GFCTB0G0.

           EXEC SQL
             SELECT  RTPO_OPER_MOTVO,
                     CIDTFD_UTILZ_ALCAD
               INTO :GFCTB0G0.RTPO-OPER-MOTVO,
                    :GFCTB0G0.CIDTFD-UTILZ-ALCAD
               FROM  DB2PRD.TPO_OPER_MOTVO
              WHERE  CTPO_OPER_MOTVO = :GFCTB0G0.CTPO-OPER-MOTVO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'GFCT0377'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TPO_OPER_MOTVO'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTF2-COD-SQL-ERRO
               MOVE '0002'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCTF2-ERRO
               MOVE 10                 TO GFCTF2-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 21000-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO GFCTF2-DESC-MSG-ERRO
               PERFORM 40000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       33000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       34000-SELECIONAR-MOVTO-ESTORNO  SECTION.
      *----------------------------------------------------------------*

           MOVE CTPO-OPER-MOTVO OF GFCTB0A5
                                       TO CTPO-OPER-MOTVO OF GFCTB089.
           MOVE CMOTVO-JUSTF    OF GFCTB0A5
                                       TO CMOTVO-JUSTF    OF GFCTB089.
           EXEC SQL
             SELECT  RMOTVO_JUSTF
               INTO :GFCTB089.RMOTVO-JUSTF
               FROM  DB2PRD.MOTVO_JUSTF_OPER
              WHERE  CTPO_OPER_MOTVO = :GFCTB089.CTPO-OPER-MOTVO AND
                     CMOTVO_JUSTF    = :GFCTB089.CMOTVO-JUSTF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'GFCT0377'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'MOTVO_JUSTF_OPER' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTF2-COD-SQL-ERRO
               MOVE '0003'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCTF2-ERRO
               MOVE 10                 TO GFCTF2-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 21000-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO GFCTF2-DESC-MSG-ERRO
               PERFORM 40000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       34000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       35000-SELECIONAR-TARIFA         SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF    OF GFCTB0A5
                                       TO CSERVC-TARIF OF GFCTB0D8.

           EXEC SQL
             SELECT  RSERVC_TARIF_REDZD
               INTO :GFCTB0D8.RSERVC-TARIF-REDZD
               FROM  DB2PRD.SERVC_TARIF_PRINC
              WHERE  CSERVC_TARIF = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'GFCT0377'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'SERVC_TARIF_PRINC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTF2-COD-SQL-ERRO
               MOVE '0004'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCTF2-ERRO
               MOVE 10                 TO GFCTF2-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 21000-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO GFCTF2-DESC-MSG-ERRO
               PERFORM 40000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       35000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

RIC   *----------------------------------------------------------------*
RIC    36000-OBTER-DESC-FUNC        SECTION.
RIC   *----------------------------------------------------------------*
RIC
RIC        MOVE WRK-COD-FUNC           TO WRK-PESQUISA-ENT.
RIC        MOVE 002                    TO WRK-CAMPO-ENT(1).
RIC        MOVE 086                    TO WRK-TAMANHO-ENT.
RIC        MOVE 'SARH8000'             TO WRK-MODULO.
RIC
RIC        CALL WRK-MODULO             USING WRK-ENTRADA
RIC                                          WRK-SAIDA
RIC                                          WRK-ERRO-AREA
RIC                                          WRK-SQLCA.
RIC
RIC        IF  WRK-COD-RETORNO-SAI     EQUAL ZEROS
RIC            MOVE ZEROS              TO GFCTF2-ERRO
RIC                                       GFCTF2-COD-SQL-ERRO
RIC                                       GFCTF2-COD-MSG-ERRO
RIC            MOVE 'PROCESSAMENTO NORMAL'
RIC                                    TO GFCTF2-DESC-MSG-ERRO
RIC            MOVE WRK-REGISTRO-SAI   TO WRK-DESC-FUNCIONARIO
RIC        ELSE
RIC            MOVE SPACES             TO WRK-DESC-FUNCIONARIO
RIC        END-IF.
RIC
RIC   *----------------------------------------------------------------*
RIC    36000-99-FIM.                  EXIT.
RIC   *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       40000-FINALIZAR                 SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       40000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
