      *================================================================*
      *  RESOLUCAO 4734                                                *
      *----------------------------------------------------------------*
      *  NOME DO BOOK: BVVEW525 - AREA DO MODULO BVVE0526              *
      *  DESCRICAO...: RETORNO DE CESSAO DE RECEBIVEIS                 *
      *                OBS. BASEADO NO I#BVVENE                        *
      *----------------------------------------------------------------*
      *  ENTRADA.....:                                                 *
      *  PROTOCOLO...: RETORNADO PELO MODULO BVVEA524 NA SOLICITACAO   *
      *                DE CESSAO                                       *
      *----------------------------------------------------------------*
      *  SAIDA.......:                                                 *
      *  COD-RETORNO.: 00 - PROCESSAMENTO OK                           *
      *                08 - ERRO CONSISTENCIA                          *
      *                09 - SALDO INDISPONIVEL                         *
      *                11 - ERRO DE COMUNICACAO COM A CIP              *
      *                13 - SOLICITACAO REJEITADA                      *
      *                12 - ERRO DE DB2                                *
      *  MSG-RETORNO.: MENSAGEM DE RETORNO                             *
      *  COD-SIT.....: ACEITO 'A' OU REJEITADO 'R'                     *
      *  MOT-REJ.....: MOTIVO REJEICAO CIP                             *
      *  ID-OPER.....: IDENTIFICADOR OPERACAO GERADO PELA CIP QUANDO   *
      *                A OPERACAO FOR ACEITA                           *
      *================================================================*
      *
       05 BVVEW525-HEADER.
          10 BVVEW525-COD-LAYOUT    PIC X(008) VALUE 'BVVEW525'.
          10 BVVEW525-TAM-LAYOUT    PIC 9(005) VALUE 00196.
      *
       05 BVVEW525-REGISTRO.
          10 BVVEW525-BLOCO-ENTRADA.
             15 BVVEW525-E-PROTOCOLO           PIC  X(050).
      *
          10 BVVEW525-BLOCO-SAIDA.
             15 BVVEW525-S-COD-RETORNO         PIC  9(002).
             15 BVVEW525-S-MSG-RETORNO         PIC  X(050).
             15 BVVEW525-S-COD-SIT             PIC  X(001).
             15 BVVEW525-S-MOT-REJ             PIC  X(060).
             15 BVVEW525-S-ID-OPER-CIP         PIC  X(020).
