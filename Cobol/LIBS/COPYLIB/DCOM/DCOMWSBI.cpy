      *================================================================*
      * NOME BOOK : DCOMWSBI                                           *
      * DESCRICAO : AREA DE ACESSO AO MODULO PARA O ROTEADOR X IMS     *
      * DATA      : 24/05/2024                                         *
      * AUTOR     : LUCIANDRA SILVEIRA                                 *
      *----------------------------------------------------------------*
      * DCOMWSBI-E-CVERSAO             = CODIGO DA VERSAO DA BOOK      *
      * DCOMWSBI-E-CPROG-ORIGE         = CODIGO PROGRAMA ORIGEM        *
      * DCOMWSBI-E-CPROG-DSTNO         = CODIGO PROGRAMA DESTINO       *
      * DCOMWSBI-DADOS                 = AREA DE DADOS                 *
      *================================================================*
       05 DCOMWSBI-REGISTRO.
          10 DCOMWSBI-IDENTIF-IC30.
             15 DCOMWSBI-IMS-TRANSACAO PIC  X(008).
             15 DCOMWSBI-IDPGM         PIC  X(008).
          10 DCOMWSBI-IDENTIFICACAO.
             15 DCOMWSBI-E-CVERSAO     PIC  9(002).
             15 DCOMWSBI-E-TPO-CHAMADA PIC  9(001).
             15 DCOMWSBI-E-CPROG-ORIGE PIC  X(008).
             15 DCOMWSBI-E-CPROG-DSTNO PIC  X(008).
          10  DCOMWSBI-BLOCO-RETORNO.
             15 DCOMWSBI-R-RETORNO     PIC  9(002).
             15 DCOMWSBI-R-ERRO        PIC  X(004).
             15 DCOMWSBI-R-MENSAGEM    PIC  X(080).
          10 DCOMWSBI-DADOS            PIC  X(3900).
      *
