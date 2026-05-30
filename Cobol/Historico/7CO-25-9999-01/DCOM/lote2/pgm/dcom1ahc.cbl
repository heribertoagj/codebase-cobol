      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. DCOM1AHC.
       AUTHOR.     LEVI BARCIELA DA SILVA CHAVES.
      *================================================================*
      *             C A P G E M I N I  -  S I S T E M A S              *
      *----------------------------------------------------------------*
      *    PROGRAMA....: DCOM1AHC                                      *
      *    ANALISTA    : LEVI BARCIELA DA SILVA CHAVES                 *
      *    DATA........: 21/03/2023                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....: BUSCAR DADOS DO DOCUMENTO DDC                 *
      *----------------------------------------------------------------*
      *    BOOKS FUNCIONAIS:                                           *
      *    DCOMWAHE - BOOK DE ENTRADA DO SERVICO COORDENADOR           *
      *    DCOMWAHI - BOOK DE COMUNICACAO COM SERVICO FUNCIONAL        *
      *    DCOMWAHS - BOOK DE SAIDA DO SERVICO COORDENADOR             *
      *    DCOMWAHW - BOOK DE GRAVACAO DE DADOS PERSISTENTES           *
      *    DCOMW002 - BOOK DE CONTROLE DO ACESSO AO SERVICO FUNCIONAL  *
      *----------------------------------------------------------------*
      *    BOOKS ARQUITETURAIS:                                        *
      *    I#FRWKAQ COMMAREA ENTRE PROGRAMA/FRAMEWORK                  *
      *    I#FRWK04 COMMAREA ENTRE PROGRAMA/FRWK1200(GAM)              *
      *    FRWKWAAA COMMAREA ENTRE PROGRAMA/FRWK1200 - DADOS SESSAO    *
      *    I#FRWKGE COMMAREA FRWK1999 (LOG DE ERRO)                    *
      *    I#FRWKMD COMMAREA FRWK1999 (LOG DE ERROS MODULO)            *
      *    I#FRWKCI COMMAREA FRWK1999 (LOG DE ERROS CICS)              *
      *----------------------------------------------------------------*
      *    MODULOS:                                                    *
      *    FRWK1999 - PROCEDIMENTOS PARA GRAVACAO DE LOGS DE ERRO.     *
      *    FRWK1200 - MODULO DE ACESSO AO GAM.                         *
      *    DCOM3AHC - MODULO FUNCIONAL ACESSADO                        *
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
       77  FILLER                      PIC  X(50)      VALUE
           '*** INICIO DA WORKING DCOM1AHC ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)      VALUE
           'AREA PARA INDEXADORES'.
      *----------------------------------------------------------------*
       01  IND-1                       PIC  9(05)   COMP-3 VALUE ZEROS.
       01  IND-2                       PIC  9(05)   COMP-3 VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)      VALUE
           'AREA PARA VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           05  WRK-FRWK1999            PIC  X(08)      VALUE 'FRWK1999'.
           05  WRK-FRWK1200            PIC  X(08)      VALUE 'FRWK1200'.
           05  WRK-PROGRAMA            PIC  X(08)      VALUE 'DCOM1AHC'.
           05  WRK-DCOM3AHC            PIC  X(08)      VALUE 'DCOM3AHC'.
           05  WRK-TRATAMENTO-ERROS.
               10  WRK-BLOCO-RETORNO.
                   15  WRK-COD-RETORNO PIC  9(02)      VALUE ZEROS.
                   15  WRK-COD-ERRO    PIC  X(04)      VALUE SPACES.
                   15  WRK-COD-MENSAGEM
                                       PIC  X(08)      VALUE SPACES.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)      VALUE
           'AREA DE COMUNICACAO DO FRWK1999 (LOG DE ERRO)'.
      *----------------------------------------------------------------*
       01  WRK-AREA-ERRO.
           COPY 'I#FRWKGE'.
           05  WRK-BLOCO-INFO-ERRO.
               10  WRK-CHAR-INFO-ERRO  PIC  X(01)
                                       OCCURS 0 TO 526 TIMES
                                       DEPENDING ON FRWKGHEA-TAM-DADOS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)      VALUE
           'AREA DE TRATAMENTO DE ERRO MODULO (API-ERRO)'.
      *----------------------------------------------------------------*
       01  WRK-COPY-MOD.
           COPY 'I#FRWKMD'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)      VALUE
           'AREA DE TRATAMENTO DE ERRO CICS(API-ERRO)'.
      *----------------------------------------------------------------*
       01  WRK-COPY-CICS.
           COPY 'I#FRWKCI'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)      VALUE
           'AREA DE TRATAMENTO DE INTERFACE (GAM)'.
      *----------------------------------------------------------------*
       01  WRK-AREA-FRWK1200.
           COPY 'I#FRWK04'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)      VALUE
           'AREA PARA BUSCAR DADOS DE SESSAO'.
      *----------------------------------------------------------------*
       01  WRK-AREA-FRWKAAA.
           COPY 'FRWKWAAA'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)      VALUE
           'AREA DE INTERFACE PARA DADOS DE SAIDA'.
      *----------------------------------------------------------------*
       01  WRK-AREA-DCOMWAHS.
           COPY DCOMWAHS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)      VALUE
           'AREA DE INTERFACE PARA DADOS DE ENTRADA'.
      *----------------------------------------------------------------*
       01  WRK-AREA-DCOMWAHE.
           COPY DCOMWAHE.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)      VALUE
           'AREA DE INTERFACE PARA DADOS DE ENTRADA'.
      *----------------------------------------------------------------*
       01  WRK-AREA-DCOMWAHW.
           COPY DCOMWAHW.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)      VALUE
           'AREA DE INTERFACE COM SERVICO FUNCIONAL - ITNET'.
      *----------------------------------------------------------------*
       01  WRK-AREA-DCOM3AHC.
           COPY DCOMW002.
           COPY DCOMWAHI.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)      VALUE
           '*** FIM DA WORKING DCOM1AHC ***'.
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       01  DFHCOMMAREA.
           COPY 'I#FRWKAQ'.

      *================================================================*
       PROCEDURE                       DIVISION  USING DFHCOMMAREA.
      *================================================================*

      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA.                               *
      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR
           PERFORM 2000-PROCESSAR
           PERFORM 3000-FINALIZAR
           .

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA INICIAL DO PROGRAMA.                                 *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                     FRWKGHEA-REGISTRO
                                          FRWKGCIC-REGISTRO
                                          FRWKGMOD-REGISTRO
                                          REPLACING NUMERIC BY ZEROS
                                               ALPHANUMERIC BY SPACES

           PERFORM 6000-OBTER-DADOS-ENTRADA
           PERFORM 1100-CONSISTIR-DADOS-ENTRADA
           PERFORM 6100-RECUPERAR-DADOS-SESSAO
           .

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA CONSISTENCIA DOS DADOS DE ENTRADA.              *
      *----------------------------------------------------------------*
       1100-CONSISTIR-DADOS-ENTRADA    SECTION.
      *----------------------------------------------------------------*

           IF (DCOMWAHE-DANO-OPER-DESC NOT NUMERIC)
           OR (DCOMWAHE-DANO-OPER-DESC EQUAL ZEROS)
               MOVE 08                 TO FRWKGLAQ-COD-RETORNO
               MOVE '0010'             TO FRWKGLAQ-COD-ERRO
               MOVE 'DCOM0029'         TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF (DCOMWAHE-NSEQ-OPER-DESC NOT NUMERIC)
           OR (DCOMWAHE-NSEQ-OPER-DESC EQUAL ZEROS)
               MOVE 08                 TO FRWKGLAQ-COD-RETORNO
               MOVE '0020'             TO FRWKGLAQ-COD-ERRO
               MOVE 'DCOM0029'         TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF (DCOMWAHE-NPCELA         NOT NUMERIC)
               MOVE 08                 TO FRWKGLAQ-COD-RETORNO
               MOVE '0021'             TO FRWKGLAQ-COD-ERRO
               MOVE 'DCOM0029'         TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA.                        *
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           EVALUATE TRUE
               WHEN FRWKWAAA-CONSULTA-INICIAL
               WHEN FRWKWAAA-PRIMEIRO-BLOCO
                   PERFORM 2100-CONSULTA-INICIAL
               WHEN FRWKWAAA-BLOCO-SEGUINTE
                   PERFORM 2200-PAGINAR
               WHEN OTHER
                   MOVE 08             TO FRWKGLAQ-COD-RETORNO
                   MOVE '0030'         TO FRWKGLAQ-COD-ERRO
                   MOVE 'DCOM0021'     TO FRWKGLAQ-COD-MENSAGEM
                   PERFORM 3000-FINALIZAR
           END-EVALUATE
           .

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------
      *    ROTINA PARA PRIMEIRA CONSULTA DO PROGRAMA
      *----------------------------------------------------------------
       2100-CONSULTA-INICIAL           SECTION.
      *----------------------------------------------------------------

           PERFORM 2210-FORMATA-COMMAREA
           PERFORM 2300-ACESSAR-FUNCIONAL
           PERFORM 2400-MOVER-DADOS-CONTRATO
           PERFORM 2500-MOVER-DADOS-PARCELAS
           PERFORM 6200-INFORMAR-BLOCO-SAIDA

           IF (DCOMW002-COD-RETORNO    EQUAL 01)
               IF (FRWKWAAA-CONSULTA-INICIAL)
                   PERFORM 6300-CRIAR-GAM-PERSISTENTE
               ELSE
                   PERFORM 6400-ATUALIZAR-GAM-PERSISTENTE
               END-IF
           END-IF
           .

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA PAGINAR A CONSULTA.
      *----------------------------------------------------------------*
       2200-PAGINAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 6600-CONSULTAR-GAM-PERSISTENTE
           PERFORM 6700-CONSISTIR-CHAVES
           PERFORM 2210-FORMATA-COMMAREA
           PERFORM 2220-FORMATA-CHAVES
           PERFORM 2300-ACESSAR-FUNCIONAL
           PERFORM 2500-MOVER-DADOS-PARCELAS
           PERFORM 6200-INFORMAR-BLOCO-SAIDA
           PERFORM 6400-ATUALIZAR-GAM-PERSISTENTE
           .

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FORMATAR O COPYBOOK DO SERVICO FUNCIONAL        *
      *----------------------------------------------------------------*
       2210-FORMATA-COMMAREA           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                     DCOMW002-BLOCO-RETORNO
                                       OF WRK-AREA-DCOM3AHC
                                          DCOMWAHI-BLOCO-ENTRADA
                                       OF WRK-AREA-DCOM3AHC
                                          DCOMWAHI-BLOCO-PAGINACAO
                                       OF WRK-AREA-DCOM3AHC
                                          REPLACING NUMERIC BY ZEROS
                                               ALPHANUMERIC BY SPACES

           MOVE FRWKWAAA-SOLIC-MAIS-DADOS
                                       TO DCOMWAHI-INDICADOR-PAGINACAO
           MOVE DCOMWAHE-DANO-OPER-DESC
                                       TO DCOMWAHI-E-DANO-OPER-DESC
           MOVE DCOMWAHE-NSEQ-OPER-DESC
                                       TO DCOMWAHI-E-NSEQ-OPER-DESC
           MOVE DCOMWAHE-NPCELA        TO DCOMWAHI-E-NPCELA
           .

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FORMATAR OS CAMPOS CHAVES PARA VALIDACAO        *
      *----------------------------------------------------------------*
       2220-FORMATA-CHAVES             SECTION.
      *----------------------------------------------------------------*

           MOVE DCOMWAHW-CHAVE-INICIO  TO DCOMWAHI-CHAVE-INICIO
           MOVE DCOMWAHW-CHAVE-FINAL   TO DCOMWAHI-CHAVE-FINAL
           .

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA ACESSO AO MODULO FUNCIONAL.                     *
      *----------------------------------------------------------------*
       2300-ACESSAR-FUNCIONAL          SECTION.
      *----------------------------------------------------------------*

           EXEC CICS LINK
                     PROGRAM (WRK-DCOM3AHC)
                     COMMAREA (WRK-AREA-DCOM3AHC)
                     LENGTH (LENGTH OF WRK-AREA-DCOM3AHC)
                     NOHANDLE
           END-EXEC

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE 16                 TO FRWKGLAQ-COD-RETORNO
               MOVE '0040'             TO FRWKGLAQ-COD-ERRO
               MOVE 'DCOM0008'         TO FRWKGLAQ-COD-MENSAGEM
               MOVE '2300-ACESSAR-FUNCIONAL'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9400-ERRO-CICS
           END-IF.

           EVALUATE DCOMW002-COD-RETORNO
                                       OF WRK-AREA-DCOM3AHC
             WHEN 00
             WHEN 01
                  MOVE DCOMW002-COD-RETORNO
                                       OF WRK-AREA-DCOM3AHC
                                       TO FRWKGLAQ-COD-RETORNO
                  MOVE DCOMW002-COD-ERRO
                                       OF WRK-AREA-DCOM3AHC
                                       TO FRWKGLAQ-COD-ERRO
                  MOVE DCOMW002-COD-MENSAGEM
                                       OF WRK-AREA-DCOM3AHC
                                       TO FRWKGLAQ-COD-MENSAGEM

             WHEN 08
                  MOVE DCOMW002-COD-RETORNO
                                      OF WRK-AREA-DCOM3AHC
                                      TO FRWKGLAQ-COD-RETORNO
                  MOVE DCOMW002-COD-ERRO
                                      OF WRK-AREA-DCOM3AHC
                                      TO FRWKGLAQ-COD-ERRO
                  MOVE DCOMW002-COD-MENSAGEM
                                      OF WRK-AREA-DCOM3AHC
                                      TO FRWKGLAQ-COD-MENSAGEM
                  PERFORM 3000-FINALIZAR

             WHEN OTHER
                  MOVE 16              TO FRWKGLAQ-COD-RETORNO
                  MOVE '0050'          TO FRWKGLAQ-COD-ERRO
                  MOVE DCOMW002-COD-MENSAGEM
                                       OF WRK-AREA-DCOM3AHC
                                       TO FRWKGLAQ-COD-MENSAGEM
                  MOVE WRK-DCOM3AHC    TO FRWKGMOD-NOME-MODULO
                  MOVE DCOMW002-BLOCO-RETORNO
                                       OF WRK-AREA-DCOM3AHC
                                       TO WRK-BLOCO-RETORNO
                  MOVE '2300-ACESSAR-FUNCIONAL'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9300-ERRO-MODULO
           END-EVALUATE
           .

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA RESPONSAVEL POR FORMATAR DADOS FIXOS DE SAIDA        *
      *----------------------------------------------------------------*
       2400-MOVER-DADOS-CONTRATO       SECTION.
      *----------------------------------------------------------------*

           MOVE DCOMWAHI-S-IPSSOA-DESC-COML
                                       TO DCOMWAHS-S-IPSSOA-DESC-COML
           MOVE DCOMWAHI-S-ISPROD-DESC-COML
                                       TO DCOMWAHS-S-ISPROD-DESC-COML
           MOVE DCOMWAHI-S-DANO-OPER-DESC
                                       TO DCOMWAHS-S-DANO-OPER-DESC
           MOVE DCOMWAHI-S-NSEQ-OPER-DESC
                                       TO DCOMWAHS-S-NSEQ-OPER-DESC
           MOVE DCOMWAHI-S-SIT-PAGAMENTO
                                       TO DCOMWAHS-S-SIT-PAGAMENTO
           MOVE DCOMWAHI-S-DVCTO-FNAL-OPER
                                       TO DCOMWAHS-S-DVCTO-FNAL-OPER
           MOVE DCOMWAHI-S-PTX-JURO-MES
                                       TO DCOMWAHS-S-PTX-JURO-MES
           MOVE DCOMWAHI-S-PTX-JURO-ANO
                                       TO DCOMWAHS-S-PTX-JURO-ANO
           MOVE DCOMWAHI-S-VCUSTO-DESC-MES
                                       TO DCOMWAHS-S-VCUSTO-DESC-MES
           MOVE DCOMWAHI-S-VCUSTO-DESC-ANO
                                       TO DCOMWAHS-S-VCUSTO-DESC-ANO
           MOVE DCOMWAHI-S-VJURO-OPER-DESC
                                       TO DCOMWAHS-S-VJURO-OPER-DESC
           MOVE DCOMWAHI-S-VIOF-OPER-DESC
                                       TO DCOMWAHS-S-VIOF-OPER-DESC
           MOVE DCOMWAHI-S-VTARIF-REG-TITLO
                                       TO DCOMWAHS-S-VTARIF-REG-TITLO
4S2511*    MOVE DCOMWAHI-S-CCNPJ-CPF   TO DCOMWAHS-S-CCNPJ-CPF
4S2511*    MOVE DCOMWAHI-S-CFLIAL-CPF  TO DCOMWAHS-S-CFLIAL-CPF
4S2511*    MOVE DCOMWAHI-S-CCTRL-CNPJ-CPF
4S2511*                                   TO DCOMWAHS-S-CCTRL-CNPJ-CPF
4S2511     IF DCOMWAHI-S-CCNPJ-CPF    EQUAL SPACES OR
4S2511        DCOMWAHI-S-CCNPJ-CPF    EQUAL LOW-VALUES OR
4S2511        DCOMWAHI-S-CFLIAL-CPF   EQUAL SPACES OR
4S2511        DCOMWAHI-S-CFLIAL-CPF   EQUAL LOW-VALUES
4S2511         MOVE SPACES            TO DCOMWAHS-S-CCNPJ-CPF
4S2511         MOVE SPACES            TO DCOMWAHS-S-CFLIAL-CPF
4S2511         MOVE ZEROS             TO DCOMWAHS-S-CCTRL-CNPJ-CPF
4S2511     ELSE
4S2511         MOVE DCOMWAHI-S-CCNPJ-CPF   TO DCOMWAHS-S-CCNPJ-CPF
4S2511         MOVE DCOMWAHI-S-CFLIAL-CPF  TO DCOMWAHS-S-CFLIAL-CPF
4S2511         MOVE DCOMWAHI-S-CCTRL-CNPJ-CPF
4S2511                                     TO DCOMWAHS-S-CCTRL-CNPJ-CPF
4S2511     END-IF
           MOVE DCOMWAHI-S-VOPER-DESC-COML
                                       TO DCOMWAHS-S-VOPER-DESC-COML
           MOVE DCOMWAHI-S-VLR-TOT-LIQ TO DCOMWAHS-S-VLR-TOT-LIQ
           MOVE DCOMWAHI-S-VLIBRC-DESC-COML
                                       TO DCOMWAHS-S-VLIBRC-DESC-COML
           MOVE DCOMWAHI-S-PERC-LIB    TO DCOMWAHS-S-PERC-LIB
           MOVE DCOMWAHI-S-DINIC-OPER-DESC
                                       TO DCOMWAHS-S-DINIC-OPER-DESC
           MOVE DCOMWAHI-S-DLIBRC-DESC-COML
                                       TO DCOMWAHS-S-DLIBRC-DESC-COML
           MOVE DCOMWAHI-S-CTPO-DESC-COML
                                       TO DCOMWAHS-S-CTPO-DESC-COML
           MOVE DCOMWAHI-S-ITPO-DESC-COML
                                       TO DCOMWAHS-S-ITPO-DESC-COML
           MOVE DCOMWAHI-S-LIBERACAO-CALC
                                       TO DCOMWAHS-S-LIBERACAO-CALC
           MOVE DCOMWAHI-S-SALDO-DEVEDOR
                                       TO DCOMWAHS-S-SALDO-DEVEDOR
           MOVE DCOMWAHI-S-TMED-PONDE-OPER
                                       TO DCOMWAHS-S-TMED-PONDE-OPER
           MOVE DCOMWAHI-S-QPCELA-OPER TO DCOMWAHS-S-QPCELA-OPER
           MOVE DCOMWAHI-S-REGISTROS   TO DCOMWAHS-S-REGISTROS
           MOVE DCOMWAHI-S-PERC-REGISTROS
                                       TO DCOMWAHS-S-PERC-REGISTROS
           MOVE DCOMWAHI-S-TRIBUTOS    TO DCOMWAHS-S-TRIBUTOS
           MOVE DCOMWAHI-S-PERC-TRIBUTOS
                                       TO DCOMWAHS-S-PERC-TRIBUTOS
           MOVE DCOMWAHI-S-TARIFAS     TO DCOMWAHS-S-TARIFAS
           MOVE DCOMWAHI-S-PERC-TARIFAS
                                       TO DCOMWAHS-S-PERC-TARIFAS
           MOVE DCOMWAHI-S-PGTO-SERVICOS
                                       TO DCOMWAHS-S-PGTO-SERVICOS
           MOVE DCOMWAHI-S-PERC-PGTO-SERVICOS
                                       TO DCOMWAHS-S-PERC-PGTO-SERVICOS
           MOVE DCOMWAHI-S-TOPER-DESC-COML
                                       TO DCOMWAHS-S-TOPER-DESC-COML
           MOVE DCOMWAHI-S-PRAZO-REMANECENTE
                                       TO DCOMWAHS-S-PRAZO-REMANECENTE
           MOVE DCOMWAHI-S-VSDO-DVDOR-ATU
                                       TO DCOMWAHS-S-VSDO-DVDOR-ATU
           MOVE DCOMWAHI-S-VPCELA-PAGAS
                                       TO DCOMWAHS-S-VPCELA-PAGAS
           MOVE DCOMWAHI-S-VPCELA-ATRASO
                                       TO DCOMWAHS-S-VPCELA-ATRASO
           MOVE DCOMWAHI-S-VSDO-LIQUIDACAO
                                       TO DCOMWAHS-S-VSDO-LIQUIDACAO
           .

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA RESPONSAVEL POR FORMATAR DADOS VARIAVEIS DE SAIDA    *
      *----------------------------------------------------------------*
       2500-MOVER-DADOS-PARCELAS       SECTION.
      *----------------------------------------------------------------*

           MOVE DCOMWAHI-S-QTDE-OCORR  TO DCOMWAHS-S-QTDE-OCORR
           MOVE DCOMWAHI-S-QTDE-OCORR  TO IND-2

           PERFORM VARYING IND-1       FROM 1 BY 1
                     UNTIL IND-1       GREATER IND-2

                MOVE DCOMWAHI-S-NPCELA-NTITLO (IND-1)
                                       TO DCOMWAHS-S-NPCELA-NTITLO
                                                                 (IND-1)
                MOVE DCOMWAHI-S-IPSSOA-SACDO (IND-1)
                                       TO DCOMWAHS-S-IPSSOA-SACDO
                                                                 (IND-1)
4S2511*         MOVE DCOMWAHI-S-CCNPJ-CPF-SAC (IND-1)
4S2511*                                TO DCOMWAHS-S-CCNPJ-CPF-SAC
4S2511*                                                          (IND-1)
4S2511*         MOVE DCOMWAHI-S-CFLIAL-CPF-SAC (IND-1)
4S2511*                                TO DCOMWAHS-S-CFLIAL-CPF-SAC
4S2511*                                                          (IND-1)
4S2511*         MOVE DCOMWAHI-S-CCTRL-CNPJ-CPF-SAC (IND-1)
4S2511*                                TO DCOMWAHS-S-CCTRL-CNPJ-CPF-SAC
4S2511*                                                          (IND-1)
4S2511          IF DCOMWAHI-S-CCNPJ-CPF-SAC (IND-1) EQUAL SPACES OR
4S2511             DCOMWAHI-S-CCNPJ-CPF-SAC (IND-1) EQUAL LOW-VALUES OR
4S2511             DCOMWAHI-S-CFLIAL-CPF-SAC (IND-1) EQUAL SPACES OR
4S2511             DCOMWAHI-S-CFLIAL-CPF-SAC (IND-1) EQUAL LOW-VALUES
4S2511              MOVE SPACES    TO DCOMWAHS-S-CCNPJ-CPF-SAC 
                                      (IND-1)
4S2511              MOVE SPACES    TO DCOMWAHS-S-CFLIAL-CPF-SAC 
                                     (IND-1)
4S2511              MOVE ZEROS     TO DCOMWAHS-S-CCTRL-CNPJ-CPF-SAC 
                                      (IND-1)
4S2511          ELSE
4S2511              MOVE DCOMWAHI-S-CCNPJ-CPF-SAC (IND-1)
4S2511                             TO DCOMWAHS-S-CCNPJ-CPF-SAC (IND-1)
4S2511              MOVE DCOMWAHI-S-CFLIAL-CPF-SAC (IND-1)
4S2511                                  TO DCOMWAHS-S-CFLIAL-CPF-SAC 
                                           (IND-1)
4S2511              MOVE DCOMWAHI-S-CCTRL-CNPJ-CPF-SAC (IND-1)
4S2511                                  TO DCOMWAHS-S-CCTRL-CNPJ-CPF-SAC 
                                            (IND-1)
4S2511          END-IF
                MOVE DCOMWAHI-S-DVCTO-PCELA-DESC (IND-1)
                                       TO DCOMWAHS-S-DVCTO-PCELA-DESC
                                                                 (IND-1)
                MOVE DCOMWAHI-S-VPRINC-PCELA-DESC (IND-1)
                                       TO DCOMWAHS-S-VPRINC-PCELA-DESC
                                                                 (IND-1)
                MOVE DCOMWAHI-S-VJURO-PCELA-DESC (IND-1)
                                       TO DCOMWAHS-S-VJURO-PCELA-DESC
                                                                 (IND-1)
                MOVE DCOMWAHI-S-VIOF-PCELA-DESC (IND-1)
                                       TO DCOMWAHS-S-VIOF-PCELA-DESC
                                                                 (IND-1)
                MOVE DCOMWAHI-S-DEFETV-BAIXA-PCELA (IND-1)
                                       TO DCOMWAHS-S-DEFETV-BAIXA-PCELA
                                                                 (IND-1)
                MOVE DCOMWAHI-S-VEFETV-BAIXA-PCELA (IND-1)
                                       TO DCOMWAHS-S-VEFETV-BAIXA-PCELA
                                                                 (IND-1)
                MOVE DCOMWAHI-S-VPRINC-PCELA-CALC (IND-1)
                                       TO DCOMWAHS-S-VPRINC-PCELA-CALC
                                                                 (IND-1)
                MOVE DCOMWAHI-S-TIPO-PAGADOR (IND-1)
                                       TO DCOMWAHS-S-TIPO-PAGADOR
                                                                 (IND-1)
                MOVE DCOMWAHI-S-IRSUMO-SIT-DESC (IND-1)
                                       TO DCOMWAHS-S-IRSUMO-SIT-DESC
                                                                 (IND-1)
           END-PERFORM
           .

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZAR O PROCESSAMENTO.                      *
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           EXEC CICS RETURN END-EXEC
           .

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    EFETUAR CHAMADA AO MODULO FRWK1200 - GAM                    *
      *----------------------------------------------------------------*
       5000-ACESSAR-GAM                SECTION.
      *----------------------------------------------------------------*

           EXEC CICS LINK
                     PROGRAM (WRK-FRWK1200)
                     COMMAREA (WRK-AREA-FRWK1200)
                     LENGTH (LENGTH OF WRK-AREA-FRWK1200)
                     NOHANDLE
           END-EXEC

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE '0060'             TO FRWKGLAQ-COD-ERRO
               MOVE 'DCOM0067'         TO FRWKGLAQ-COD-MENSAGEM
               MOVE '5000-ACESSAR-GAM' TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9400-ERRO-CICS
           END-IF.

           IF (FRWKEL04-COD-RETORNO    NOT EQUAL ZEROES)
               MOVE 16                 TO FRWKGLAQ-COD-RETORNO
               MOVE '0070'             TO FRWKGLAQ-COD-ERRO
               MOVE FRWKEL04-COD-MENSAGEM
                                       TO FRWKGLAQ-COD-MENSAGEM
               MOVE WRK-FRWK1200       TO FRWKGMOD-NOME-MODULO
               MOVE FRWKEL04-BLOCO-RETORNO
                                       TO FRWKGMOD-BLOCO-RETORNO
               MOVE '5000-ACESSAR-GAM' TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9300-ERRO-MODULO
           END-IF
           .

      *----------------------------------------------------------------*
       5000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA OBTER OS DADOS DE ENTRADA.                      *
      *----------------------------------------------------------------*
       6000-OBTER-DADOS-ENTRADA        SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                     FRWKEL04-BLOCO-ENTRADA
                                          FRWKEL04-BLOCO-RETORNO
                                          REPLACING NUMERIC BY ZEROS
                                               ALPHANUMERIC BY SPACES

           SET  FRWKEL04-FUN-CONSULTAR TO TRUE
           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO FRWKEL04-IDENT-AREA-GLOBAL
           MOVE DCOMWAHE-COD-LAYOUT    TO FRWKEL04-ID-BLOCO
           MOVE DCOMWAHE-TAM-LAYOUT    TO FRWKEL04-TAM-BLOCO

           PERFORM 5000-ACESSAR-GAM

           MOVE FRWKEL04-BUFFER          (1:LENGTH OF WRK-AREA-DCOMWAHE)
                                       TO WRK-AREA-DCOMWAHE.
           

      *----------------------------------------------------------------*
       6000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA RECUPERAR DADOS DE SESSAO                       *
      *----------------------------------------------------------------*
       6100-RECUPERAR-DADOS-SESSAO     SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                     FRWKEL04-BLOCO-ENTRADA
                                          FRWKEL04-BLOCO-RETORNO
                                          REPLACING NUMERIC BY ZEROS
                                               ALPHANUMERIC BY SPACES

           SET FRWKEL04-FUN-CONSULTAR  TO TRUE
           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO FRWKEL04-IDENT-AREA-GLOBAL
           MOVE 'FRWKEL83'             TO FRWKEL04-ID-BLOCO
           MOVE FRWKWAAA-TAM-LAYOUT    TO FRWKEL04-TAM-BLOCO

           PERFORM 5000-ACESSAR-GAM

           MOVE FRWKEL04-BUFFER          (1:LENGTH  OF WRK-AREA-FRWKAAA)
                                       TO WRK-AREA-FRWKAAA.
           

      *----------------------------------------------------------------*
       6100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FORMATAR OS DADOS DE SAIDA                      *
      *----------------------------------------------------------------*
       6200-INFORMAR-BLOCO-SAIDA       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                     FRWKEL04-BLOCO-ENTRADA
                                          FRWKEL04-BLOCO-RETORNO
                                          REPLACING NUMERIC BY ZEROS
                                               ALPHANUMERIC BY SPACES

           SET FRWKEL04-FUN-CRIAR      TO TRUE
           SET FRWKEL04-PERM-CONSULTAR TO TRUE
           SET FRWKEL04-IND-BL-SAIDA   TO TRUE
           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO FRWKEL04-IDENT-AREA-GLOBAL
           MOVE DCOMWAHS-COD-LAYOUT    TO FRWKEL04-ID-BLOCO
           MOVE LENGTH                 OF WRK-AREA-DCOMWAHS
                                       TO FRWKEL04-TAM-BLOCO
                                          DCOMWAHS-TAM-LAYOUT
           MOVE WRK-AREA-DCOMWAHS        (1:LENGTH WRK-AREA-DCOMWAHS)
                                       TO FRWKEL04-BUFFER
           PERFORM 5000-ACESSAR-GAM
           .

      *----------------------------------------------------------------*
       6200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA CRIACAO DO BLOCO DE PERSISTENCIA                *
      *----------------------------------------------------------------*
       6300-CRIAR-GAM-PERSISTENTE      SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                     FRWKEL04-BLOCO-ENTRADA
                                          FRWKEL04-BLOCO-RETORNO
                                          REPLACING NUMERIC BY ZEROS
                                               ALPHANUMERIC BY SPACES

           SET FRWKEL04-FUN-CRIAR      TO TRUE
           SET FRWKEL04-PERM-RESTRITO  TO TRUE
           SET FRWKEL04-IND-BL-PERS    TO TRUE

           PERFORM 6500-EFETIVA-PERSISTENCIA-GAM
           .

      *----------------------------------------------------------------*
       6300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA ATUALIZACAO DO BLOCO DE PERSISTENCIA            *
      *----------------------------------------------------------------*
       6400-ATUALIZAR-GAM-PERSISTENTE  SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                     FRWKEL04-BLOCO-ENTRADA
                                          FRWKEL04-BLOCO-RETORNO
                                          REPLACING NUMERIC BY ZEROS
                                               ALPHANUMERIC BY SPACES

           SET FRWKEL04-FUN-ATUALIZAR  TO TRUE
           SET FRWKEL04-PERM-ATUALIZAR TO TRUE
           SET FRWKEL04-IND-BL-PERS    TO TRUE

           PERFORM 6500-EFETIVA-PERSISTENCIA-GAM
           .

      *----------------------------------------------------------------*
       6400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA EFETIVAR A PERSISTENCIA NO GAM                  *
      *----------------------------------------------------------------*
       6500-EFETIVA-PERSISTENCIA-GAM   SECTION.
      *----------------------------------------------------------------*

           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO FRWKEL04-IDENT-AREA-GLOBAL
           MOVE DCOMWAHW-COD-LAYOUT    TO FRWKEL04-ID-BLOCO
           MOVE DCOMWAHW-TAM-LAYOUT    TO FRWKEL04-TAM-BLOCO
           MOVE FRWKWAAA-SOLIC-MAIS-DADOS
                                       TO DCOMWAHW-INDICADOR-PAGINACAO
           MOVE DCOMWAHE-BLOCO-ENTRADA TO DCOMWAHW-BLOCO-ENTRADA
           MOVE DCOMWAHI-CHAVE-INICIO  TO DCOMWAHW-CHAVE-INICIO
           MOVE DCOMWAHI-CHAVE-FINAL   TO DCOMWAHW-CHAVE-FINAL
           MOVE DCOMW002-COD-RETORNO   TO DCOMWAHW-COD-RETORNO
           MOVE WRK-AREA-DCOMWAHW        (1:LENGTH OF WRK-AREA-DCOMWAHW)
                                       TO FRWKEL04-BUFFER

           PERFORM 5000-ACESSAR-GAM
           .

      *----------------------------------------------------------------*
       6500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA CONSULTAR O BLOCO DE PERSISTENCIA               *
      *----------------------------------------------------------------*
       6600-CONSULTAR-GAM-PERSISTENTE  SECTION.
      *----------------------------------------------------------------*

           SET  FRWKEL04-FUN-CONSULTAR TO TRUE

           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO FRWKEL04-IDENT-AREA-GLOBAL
           MOVE DCOMWAHW-COD-LAYOUT    TO FRWKEL04-ID-BLOCO
           MOVE DCOMWAHW-TAM-LAYOUT    TO FRWKEL04-TAM-BLOCO

           PERFORM 5000-ACESSAR-GAM

           MOVE FRWKEL04-BUFFER          (1:LENGTH OF WRK-AREA-DCOMWAHW)
                                       TO WRK-AREA-DCOMWAHW
           .

      *----------------------------------------------------------------*
       6600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA CONSISTENCIA DE CHAVES                          *
      *----------------------------------------------------------------*
       6700-CONSISTIR-CHAVES           SECTION.
      *----------------------------------------------------------------*

           IF ((DCOMWAHW-P-INICIAL     OR DCOMWAHW-P-PRIMEIRA)
                                       AND FRWKWAAA-BLOCO-ANTERIOR)
           OR  (DCOMWAHW-P-ULTIMA      AND FRWKWAAA-BLOCO-SEGUINTE)
           OR  (DCOMWAHW-P-SEGUINTE    AND FRWKWAAA-BLOCO-SEGUINTE
           AND DCOMWAHW-COD-RETORNO    EQUAL ZEROES)
           OR  (DCOMWAHW-P-ANTERIOR    AND FRWKWAAA-BLOCO-ANTERIOR
           AND DCOMWAHW-COD-RETORNO    EQUAL ZEROES)
               MOVE 08                 TO FRWKGLAQ-COD-RETORNO
               MOVE '0080'             TO FRWKGLAQ-COD-ERRO
               MOVE 'DCOM0074'         TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF
           .

      *----------------------------------------------------------------*
       6700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO MODULO                       *
      *----------------------------------------------------------------*
       9300-ERRO-MODULO                SECTION.
      *----------------------------------------------------------------*

           SET  ERRO-MODULO            TO TRUE
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA
           MOVE FRWKGMOD-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS
           MOVE WRK-COPY-MOD           TO WRK-BLOCO-INFO-ERRO

           PERFORM 9999-API-ERROS
           .

      *----------------------------------------------------------------*
       9300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO CICS.                        *
      *----------------------------------------------------------------*
       9400-ERRO-CICS                  SECTION.
      *----------------------------------------------------------------*

           SET  ERRO-CICS              TO TRUE
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA
           MOVE FRWKGCIC-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS
           MOVE EIBFN                  TO FRWKGCIC-EIBFN
           MOVE EIBRCODE               TO FRWKGCIC-EIBRCODE
           MOVE EIBRSRCE               TO FRWKGCIC-EIBRSRCE
           MOVE EIBRESP                TO FRWKGCIC-EIBRESP
           MOVE EIBRESP2               TO FRWKGCIC-EIBRESP2
           MOVE EIBTASKN               TO FRWKGCIC-EIBTASKN
           MOVE WRK-COPY-CICS          TO WRK-BLOCO-INFO-ERRO

           PERFORM 9999-API-ERROS
           .

      *----------------------------------------------------------------*
       9400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO CHAMADA AO FRWK1999.                 *
      *----------------------------------------------------------------*
       9999-API-ERROS                  SECTION.
      *----------------------------------------------------------------*

           EXEC CICS LINK
                     PROGRAM (WRK-FRWK1999)
                     COMMAREA (WRK-AREA-ERRO)
                     LENGTH (LENGTH OF WRK-AREA-ERRO)
                     NOHANDLE
           END-EXEC

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF.

           PERFORM 3000-FINALIZAR
           .

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
