      ******************************************************************
      *  NOVOS NORMATIVOS - RESOLUCAO 4734                             *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  NOME DO BOOK..: I#BVVENE - AREA DO MODULO BVVEA505            *
      *  DESCRICAO.....: RETORNO DE CESSAO DE RECEBIVEIS               *
      *  TAMANHO.......: 00183 BYTES                                   *
      *                                                                *
      *--ENTRADA-------------------------------------------------------*
      *                                                                *
      *  PROTOCOLO.....: RETORNADO PELO MODULO BVVEA504 NA SOLICITACAO *
      *                  DE CESSAO                                     *
      *                                                                *
      *--SAIDA---------------------------------------------------------*
      *                                                                *
      *  COD-RETORNO...: 00 - PROCESSAMENTO OK                         *
      *                  08 - ERRO CONSISTENCIA                        *
      *                  09 - SALDO INDISPONIVEL                       *
      *                  11 - ERRO DE COMUNICACAO COM A CIP            *
      *                  13 - SOLICITACAO REJEITADA                    *
      *                  16 - ERRO DE DB2                              *
      *                                                                *
      *  MSG-RETORNO...: MENSAGEM DE RETORNO                           *
      *  COD-SIT.......: ACEITO 'A' OU REJEITADO 'R'                   *
      *  MOT-REJ.......: MOTIVO REJEICAO CIP                           *
      *  ID-OPER.......: IDENTIFICADOR OPERACAO GERADO PELA CIP QUANDO *
      *                  A OPERACAO FOR ACEITA                         *
      *                                                                *
      ******************************************************************
      *                                                                *
       01  BVVENE-COMMAREA.
           05 BVVENE-AREA-ENTRADA.
              10 BVVENE-E-PROTOCOLO                    PIC  X(050).

           05 BVVENE-AREA-SAIDA.
              10 BVVENE-S-COD-RETORNO                  PIC  9(002).
              10 BVVENE-S-MSG-RETORNO                  PIC  X(050).
              10 BVVENE-S-COD-SIT                      PIC  X(001).
              10 BVVENE-S-MOT-REJ                      PIC  X(060).
              10 BVVENE-S-ID-OPER-CIP                  PIC  X(020).
