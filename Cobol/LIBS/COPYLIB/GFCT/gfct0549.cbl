      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0549.
       AUTHOR.     HELIO SANTONI.
      *================================================================*
      *      S O N D A   P R O C W O R K   O U T S O U R C I N G       *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0549                                    *
      *    PROGRAMADOR.:   HELIO SANTONI           - PROCWORK - GP.50  *
      *    ANALISTA....:   VALERIA TORQUATO        - PROCWORK - GP.50  *
      *    DATA........:   29/05/2009                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   INCLUSAO DA AUTORIZACAO DE EMISSAO DE       *
      *      EXTRATO ANUAL DE TARIFAS.                                 *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *            DB2                                                 *
      *             TABLE                             INCLUDE/BOOK     *
      *              DB2PRD.TAUTRZ_ENVIO_AGPTO          GFCTB0L4       *
      *              DB2PRD.TAUTRZ_ENVIO_CLI            GFCTB0L5       *
      *              DB2PRD.TAUTRZ_ENVIO_POSTO          GFCTB0L6       *
      *              DB2PRD.TAUTRZ_ENVIO_SGMTO          GFCTB0L7       *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    GFCTWAGO - AREA DE COMUNICACAO - ENTRADA.                   *
      *    GFCTWAGP - AREA DE COMUNICACAO - SAIDA.                     *
      *    I#GFCTG2 - ENTRADA - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTG3 - SAIDA - OBTER DESCRICAO DE MENSAGEM.             *
      *    I#GFCTFZ - ENTRADA - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG1 - SAIDA - VERIFICAR DISPONIBILIDADE DO ON-LINE.    *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5522 - MODULO PARA VERIFICAR DISPONIBILIDADE DO ON-LINE.*
      *    GFCT5523 - MODULO PARA OBTER DESCRICAO DE MENSAGENS.        *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

