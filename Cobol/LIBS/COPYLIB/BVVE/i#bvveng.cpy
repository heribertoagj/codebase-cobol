      ******************************************************************
      *  NOVOS NORMATIVOS - RESOLUCAO 4734                             *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  NOME DO BOOK....: I#BVVENG - AREA DO MODULO BVVEA507          *
      *  DESCRICAO.......: RETORNA SITUACAO DE CANCELAMENTO NA CIP     *
      *  TAMANHO.........: 00183 BYTES                                 *
      *                                                                *
      *--ENTRADA-------------------------------------------------------*
      *                                                                *
      *  PROTOCOLO.......: PROTOCOLO PARA RASTREIO DA PESQUISA. GERADO *
      *                    NO MODULO BVVEA506                          *
      *                                                                *
      *--SAIDA---------------------------------------------------------*
      *                                                                *
      *  COD-RETORNO...: 00 - PROCESSAMENTO OK                         *
      *                  08 - ERRO CONSISTENCIA                        *
      *                  11 - ERRO DE COMUNICACAO COM A CIP            *
      *                  13 - SOLICITACAO REJEITADA                    *
      *                  16 - ERRO DE DB2                              *
      *                                                                *
      *  MSG-RETORNO.....: MENSAGEM DE RETORNO                         *
      *  ID-OPER-CANC....: ID OPERACAO DE CANCELAMENTO GERADO PELA CIP *
      *  COD-SIT.........: CODIGO DA SITUACAO DA SOLICITACAO:          *
      *                    A-ACATADA / R-REJEITADA                     *
      *  MOT-REJ.........: MOTIVO DA REJEICAO DA SOLICITACAO NA CIP    *
      *                                                                *
      ******************************************************************
      *                                                                *
       01  BVVENG-COMMAREA.
           05 BVVENG-AREA-ENTRADA.
              10 BVVENG-E-PROTOCOLO                    PIC  X(050).

           05 BVVENG-AREA-SAIDA.
              10 BVVENG-S-COD-RETORNO                  PIC  9(002).
              10 BVVENG-S-MSG-RETORNO                  PIC  X(050).
              10 BVVENG-S-ID-OPER-CANC                 PIC  X(020).
              10 BVVENG-S-COD-SIT                      PIC  X(001).
              10 BVVENG-S-MOT-REJ                      PIC  X(060).
