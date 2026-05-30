      *================================================================*
      *  RESOLUCAO 4734                                                *
      *----------------------------------------------------------------*
      *  NOME DO BOOK: BVVEW523 - AREA DO MODULO BVVE0523              *
      *  DESCRICAO...: VERIFICAR SE SOLICITACAO JA FOI PROCESSADA      *
      *----------------------------------------------------------------*
      *  ENTRADA.....:                                                 *
      *  PROTOCOLO...: PROTOCOLO P/ RASTREIO DA PESQUISA, COMPOSICAO:  *
      *                CANAL X(4) + TIMESTAMP X(26) + AGENCIA 9(5) +   *
      *                CONTA 9(13) + FILLER X(3)                       *
      *----------------------------------------------------------------*
      *  SAIDA.......:                                                 *
      *  COD-RETORNO : 00 - PROCESSAMENTO OK                           *
      *                01 - EM PROCESSAMENTO                           *
      *                08 - ERRO CONSISTENCIA                          *
      *                16 - ERRO DE DB2                                *
      *  MSG-RETORNO.: MENSAGEM DE RETORNO                             *
      *  PROTOCOLO...: PROTOCOLO P/ RASTREIO DA PESQUISA               *
      *  SITUACAO....: DESCRITIVO DA SITUACAO DO PROTOCOLO             *
      *================================================================*

       05 BVVEW523-HEADER.
          10 BVVEW523-COD-LAYOUT       PIC X(008) VALUE 'BVVEW523'.
          10 BVVEW523-TAM-LAYOUT       PIC 9(005) VALUE 00195.
      *
       05 BVVEW523-REGISTRO.
          10 BVVEW523-BLOCO-ENTRADA.
             15 BVVEW523-E-PROTOCOLO   PIC  X(050).
      *
          10 BVVEW523-BLOCO-SAIDA.
             15 BVVEW523-S-COD-RETORNO PIC  9(002).
             15 BVVEW523-S-MSG-RETORNO PIC  X(050).
             15 BVVEW523-S-PROTOCOLO   PIC  X(050).
             15 BVVEW523-S-SITUACAO    PIC  X(030).
