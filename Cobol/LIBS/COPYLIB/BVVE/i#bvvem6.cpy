      ******************************************************************
      *  NOVOS NORMATIVOS - RESOLUCAO 4734                             *
      ******************************************************************
      *  NOME DO BOOK....: I#BVVEM6                                    *
      *  DESCRICAO.......: OPERACOES EM LOTE - ALTERACAO E INCLUSAO    *
      *                    PREPARA ARQUIVO PARA ENVIO DO ARRC022       *
      *                    COPIA DO BOOK I#BVVEM5 + FLAG DE ALCANCE    *
      *  TAMANHO.........: 350 BYTES                                   *
      *----------------------------------------------------------------*
      *  COD-OPERACAO : CONTRATO LOTE / CONTRATO BRADSECO              *
      *  COD-OPER-CIP : CONTRATO DEVOLVIDO PELA CIP                    *
      *  DATAS        : DD.MM.AAAA                                     *
      ******************************************************************
      *                                                                *
       01  REG-BVVEM6.
           03 BVVEM6-DADOS-CONTRATO.
              05 BVVEM6-AGENCIA                     PIC  9(005).
              05 BVVEM6-CONTA                       PIC  9(013).
              05 BVVEM6-COD-OPERACAO                PIC  X(019).
              05 BVVEM6-IND-I-A                     PIC  X(001).
              05 BVVEM6-DT-VCTO-OPER                PIC  X(010).
              05 BVVEM6-VLR-TOT-OPER                PIC  9(015)V99.
           03 BVVEM6-DADOS-UR.
              05 BVVEM6-PRODUTO                     PIC  9(005).
              05 BVVEM6-VLR-OPER-PRODUTO            PIC  9(015)V99.
              05 BVVEM6-DT-UR                       PIC  X(010).
           03 BVVEM6-CRED                           PIC  9(004).
           03 BVVEM6-CNPJ-CREDENC                   PIC  9(014).
           03 BVVEM6-BAND-VISAO                     PIC  9(003).
           03 BVVEM6-ARRANJO                        PIC  X(003).
      *    PARA CPF FORMATAR COM ESPAÁOS A DIREITA
           03 BVVEM6-CNPJ-USUAR-FNAL                PIC  X(014).
           03 BVVEM6-COD-OPER-CIP                   PIC  X(019).
           03 BVVEM6-DT-INI-GRAV                    PIC  X(010).
           03 BVVEM6-DT-FIM-GRAV                    PIC  X(010).
           03 BVVEM6-IND-RENEG                      PIC  X(001).
           03 BVVEM6-CONTRATO-RENEG                 PIC  X(019).
           03 BVVEM6-CTPO-CONTA                     PIC  X(002).
      *    PARA CPF FORMATAR COM ESPAÁOS A DIREITA
           03 BVVEM6-CPFCNPJ-TTLAR                  PIC  X(014).
           03 BVVEM6-FLAG-GERAL-ESPECIF             PIC  X(001).
           03 FILLER                                PIC  X(139).
