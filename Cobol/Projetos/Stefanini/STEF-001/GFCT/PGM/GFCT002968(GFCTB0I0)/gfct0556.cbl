      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0556.
       AUTHOR.     HELIO SANTONI NETO.
      *================================================================*
      *      S O N D A   P R O C W O R K   O U T S O U R C I N G       *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0556                                    *
      *    PROGRAMADOR.:   HELIO SANTONI           - PROCWORK /GP.50   *
      *    ANALISTA....:   VALERIA TORQUATO        - PROCWORK /GP.50   *
      *    DATA........:   01/06/2009                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   EXCLUSAO DO CANCELAMENTO DE EMISSAO DE      *
      *      EXTRATO ANUAL DE TARIFAS.                                 *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *            DB2                                                 *
      *             TABLE                             INCLUDE/BOOK     *
      *             DB2PRD.TAUTRZ_ENVIO_AGPTO           GFCTB0L4       *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    GFCTWAG4 - AREA DE COMUNICACAO - ENTRADA.                   *
      *    GFCTWAG5 - AREA DE COMUNICACAO - SAIDA.                     *
      *    I#GFCTG2 - ENTRADA - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTG3 - SAIDA   - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTFZ - ENTRADA - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG1 - SAIDA   - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5522 - MODULO PARA VERIFICAR DISPONIBILIDADE DO ON-LINE.*
      *    GFCT5523 - MODULO PARA OBTER DESCRICAO DE MENSAGENS.        *
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
       77  FILLER                      PIC  X(033)         VALUE
           'INICIO DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(030)         VALUE
           'AREA PARA VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-NOM-TAB                 PIC  X(008)         VALUE SPACES.

       01  WRK-MSG001.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA DO MODULO '.
           05 WRK-MODULO-MSG           PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(041)         VALUE SPACES.

       01  WRK-MSG002                  PIC  X(075)         VALUE
           'SISTEMA INDISPONIVEL'.

       01  WRK-AUX-7.
           05 WRK-AUX-7-N              PIC  9(007)         VALUE ZEROS.

       01  WRK-AUX-9-S                 PIC S9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-AUX-9-S.
           05 WRK-AUX-9                PIC  9(009).
           05 FILLER                   REDEFINES WRK-AUX-9.
             10 FILLER                 PIC  X(006).
             10 WRK-AUX-9-3-A.
               15 WRK-AUX-9-3          PIC  9(003).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           'AREA DE COMUNICACAO COM GFCT5522'.
      *----------------------------------------------------------------*

       COPY I#GFCTFZ.

       COPY I#GFCTG1.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           'AREA DE COMUNICACAO COM GFCT5523'.
      *----------------------------------------------------------------*

       COPY I#GFCTG2.

       COPY I#GFCTG3.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(013)         VALUE
           'AREA PARA DB2'.
      *----------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0L4
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(030)         VALUE
           'FIM DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY GFCTWAG4.

       COPY GFCTWAG5.

       COPY I#GFCT0M.

      *================================================================*
       PROCEDURE                       DIVISION USING WAG4-ENTRADA
                                                      WAG5-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *CHAMA AS ROTINAS PRINCIPAIS DO PROCESSAMENTO                    *
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
      *PROCEDIMENTOS INICIAIS DO PROGRAMA                              *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          WAG5-SAIDA.
           INITIALIZE WAG5-SAIDA
                      GFCT0M-AREA-ERROS.

           PERFORM 1100-MONTAR-AREA-SAIDA.

           PERFORM 1200-VERIFICAR-DISP-SISTEMA.

           PERFORM 1300-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *MONTA AREA DE SAIDA.                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-MONTAR-AREA-SAIDA          SECTION.
      *----------------------------------------------------------------*

           MOVE +200                   TO WAG5-LL.
           MOVE ZEROS                  TO WAG5-ZZ.
           MOVE WAG4-TRANSACAO         TO WAG5-TRANSACAO.
           MOVE WAG4-FUNCAO            TO WAG5-FUNCAO.

           MOVE WAG4-FUNC-BDSCO        TO WAG5-FUNC-BDSCO.
           MOVE 'S'                    TO WAG5-FIM.
           MOVE ZEROS                  TO WAG5-ERRO
                                          WAG5-COD-SQL-ERRO
                                          WAG5-COD-MSG-ERRO.
           MOVE '0000'                 TO GFCTG2-COD-MSG.

           PERFORM 1110-OBTER-DESC-MSG.

           MOVE GFCTG3-DESC-MSG        TO WAG5-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTEM DESCRICAO DA MENSAGEM - GFCT5523                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                     TO GFCTG3-SAIDA.
           INITIALIZE GFCTG3-SAIDA.

           MOVE +100                       TO GFCTG2-LL.
           MOVE ZEROS                      TO GFCTG2-ZZ.
           MOVE WAG4-TRANSACAO             TO GFCTG2-TRANSACAO.
           MOVE WAG4-FUNCAO                TO GFCTG2-FUNCAO.
           MOVE WAG4-FUNC-BDSCO            TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                        TO GFCTG2-TIPO-PROC.
           MOVE 'N'                        TO GFCTG2-FIM.
           MOVE 'GFCT5523'                 TO WRK-MODULO.

           CALL WRK-MODULO              USING GFCTG2-ENTRADA
                                              GFCTG3-SAIDA
                                              GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                NOT EQUAL ZEROS
               MOVE 9                      TO WAG5-ERRO
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
                                              WRK-MODULO-MSG
               MOVE WRK-MSG001             TO GFCT0M-TEXTO
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0556'             TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO              EQUAL 9
               IF  GFCT0M-TIPO-ACESSO     NOT EQUAL 'DB2'
                   MOVE GFCTG3-DESC-MSG-ERRO(1:75)
                                           TO GFCT0M-TEXTO
                   MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               ELSE
                   MOVE GFCTG3-COD-SQL-ERRO
                                           TO WAG5-COD-SQL-ERRO
               END-IF
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
               MOVE 'GFCT0556'             TO GFCT0M-TRANSACAO
               MOVE GFCTG3-ERRO            TO WAG5-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO              EQUAL 1
               MOVE 1                      TO WAG5-ERRO
               MOVE GFCTG3-COD-SQL-ERRO    TO WAG5-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO    TO WAG5-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO   TO WAG5-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICA DISPONIBILIDADE DO SISTEMA                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-VERIFICAR-DISP-SISTEMA     SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                     TO GFCTG1-SAIDA.
           INITIALIZE GFCTG1-SAIDA.

           MOVE +100                       TO GFCTFZ-LL.
           MOVE ZEROS                      TO GFCTFZ-ZZ.
           MOVE WAG4-TRANSACAO             TO GFCTFZ-TRANSACAO.
           MOVE WAG4-FUNCAO                TO GFCTFZ-FUNCAO.
           MOVE WAG4-FUNC-BDSCO            TO GFCTFZ-FUNC-BDSCO.
           MOVE 'GFCT'                     TO GFCTFZ-SISTEMA.
           MOVE 'N'                        TO GFCTFZ-FIM.
           MOVE 'GFCT5522'                 TO WRK-MODULO.

           CALL WRK-MODULO              USING GFCTFZ-ENTRADA
                                              GFCTG1-SAIDA
                                              GFCT0M-AREA-ERROS.

           IF  RETURN-CODE          NOT EQUAL ZEROS
               MOVE 9                      TO WAG5-ERRO
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
                                              WRK-MODULO-MSG
               MOVE WRK-MSG001             TO GFCT0M-TEXTO
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0556'             TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO              EQUAL 9
               IF  GFCT0M-TIPO-ACESSO     NOT EQUAL 'DB2'
                   MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
                   MOVE GFCTG1-DESC-MSG-ERRO(1:75)
                                           TO GFCT0M-TEXTO
               ELSE
                   MOVE GFCTG1-COD-SQL-ERRO
                                           TO WAG5-COD-SQL-ERRO
               END-IF
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
               MOVE 'GFCT0556'             TO GFCT0M-TRANSACAO
               MOVE GFCTG1-ERRO            TO WAG5-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE       EQUAL 1
               MOVE 9                      TO WAG5-ERRO
               MOVE WRK-MSG002             TO GFCT0M-TEXTO
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0556'             TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTE DADOS RECEBIDOS NA ENTRADA                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (WAG4-TRANSACAO          EQUAL SPACES  OR LOW-VALUES) OR
              (WAG4-FUNCAO             EQUAL SPACES  OR LOW-VALUES) OR
              (WAG4-FUNC-BDSCO         EQUAL SPACES  OR LOW-VALUES) OR
              (WAG4-CAGPTO             NOT NUMERIC)  OR
              (WAG4-CAGPTO             EQUAL ZEROS)  OR
              (WAG4-JUSTIF             EQUAL SPACES)
               MOVE  1                 TO WAG5-ERRO
                                          WAG5-COD-MSG-ERRO
               MOVE  ZEROS             TO WAG5-COD-SQL-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO WAG5-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAMENTO PRINCIPAL                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-CANCELAR-AUTORIZACAO.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ATUALIZA REGISTRO DE GFCTB0L4 EXCLUSAO LOGICA                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-CANCELAR-AUTORIZACAO       SECTION.
      *----------------------------------------------------------------*

           MOVE WAG4-HINCL             TO HINCL-REG-SIST     OF GFCTB0L4
           MOVE WAG4-CAGPTO            TO CAGPTO-CTA         OF GFCTB0L4
           MOVE 2                      TO CINDCD-AUTRZ-EMIS  OF GFCTB0L4
           MOVE WAG4-FUNC-BDSCO        TO WRK-AUX-7
           MOVE WRK-AUX-7-N            TO CFUNC-EXCL-SIST    OF GFCTB0L4
           MOVE WAG4-JUSTIF            TO RJUSTF-EXCL-AUTRZ-TEXT
                                                             OF GFCTB0L4
           MOVE LENGTH OF RJUSTF-EXCL-AUTRZ-TEXT             OF GFCTB0L4
                                       TO RJUSTF-EXCL-AUTRZ-LEN
                                                             OF GFCTB0L4

           EXEC SQL
             UPDATE DB2PRD.TAUTRZ_ENVIO_AGPTO
                SET RJUSTF_EXCL_AUTRZ  = :GFCTB0L4.RJUSTF-EXCL-AUTRZ,
                    CFUNC_EXCL_SIST    = :GFCTB0L4.CFUNC-EXCL-SIST ,
                    HEXCL_REG_SIST     =  CURRENT TIMESTAMP
              WHERE CAGPTO_CTA         = :GFCTB0L4.CAGPTO-CTA
                AND CINDCD_AUTRZ_EMIS  = :GFCTB0L4.CINDCD-AUTRZ-EMIS
                AND HINCL_REG_SIST     = :GFCTB0L4.HINCL-REG-SIST
           END-EXEC.

           IF  (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0                   EQUAL 'W')
                MOVE '0010'                TO GFCTG2-COD-MSG
                PERFORM 1110-OBTER-DESC-MSG
                INITIALIZE GFCT0M-ERRO-SQL
                MOVE 'GFCTB0L4'            TO WRK-NOM-TAB
                MOVE  0010                 TO WAG5-COD-MSG-ERRO
                MOVE 'TAUTRZ_ENVIO_AGPTO'  TO GFCT0M-NOME-TAB
                MOVE 'UPDATE  '            TO GFCT0M-COMANDO-SQL
                MOVE  '0010'               TO GFCT0M-LOCAL
                PERFORM 2110-CONCATENAR-MENSAGEM
                PERFORM 2120-FORMATAR-ERRO-DB2
                PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL +100
               MOVE 1                  TO WAG5-ERRO
               MOVE ZEROS              TO WAG5-COD-SQL-ERRO
               MOVE 1502               TO WAG5-COD-MSG-ERRO
               MOVE '1502'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO WAG5-DESC-MSG-ERRO(1:70)
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ORGANIZA A MENSAGEM COM O NOME DA TABELA EM QUE OCORREU ERRO. *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-CONCATENAR-MENSAGEM        SECTION.
      *----------------------------------------------------------------*

           STRING GFCTG3-DESC-MSG ' - ' WRK-NOM-TAB
           DELIMITED BY '  '         INTO WAG5-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * FORMATA VARIAVEIS COMUNS A TODOS OS ERROS DB2 DO PROGRAMA.     *
      ******************************************************************
      *----------------------------------------------------------------*
       2120-FORMATAR-ERRO-DB2          SECTION.
      *----------------------------------------------------------------*

           MOVE  9                     TO WAG5-ERRO.
           MOVE 'GFCT0556'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE  SQLCA                 TO GFCT0M-SQLCA-AREA.
           MOVE  SQLCODE               TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
           MOVE  WRK-AUX-9-3           TO WAG5-COD-SQL-ERRO.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FINALIZA O PROCESSAMENTO E RETORNA AO PROGRAMA CHAMADOR.        *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WAG5-ERRO        NOT EQUAL ZEROS
               MOVE WAG4-HINCL         TO WAG5-HINCL
               MOVE WAG4-CAGPTO        TO WAG5-CAGPTO
               MOVE WAG4-CSEGMTO       TO WAG5-CSEGMTO
               MOVE WAG4-CAGEN         TO WAG5-CAGEN
               MOVE WAG4-CPAB          TO WAG5-CPAB
               MOVE WAG4-CCNPJ-CPF     TO WAG5-CNPJ-CPF
               MOVE WAG4-JUSTIF        TO WAG5-JUSTIF
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
