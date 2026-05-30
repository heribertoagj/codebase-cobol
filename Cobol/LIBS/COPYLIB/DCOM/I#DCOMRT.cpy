      *************************************************************
      * SISTEMA : DESCONTO                                        *
      * ARRQUIVO: ARQUIVO DE TRANSMISSAO COBRANCA, COM DADOS DO   *
      *           DESCONTO (REMESSA CNAB)                         *
      * INC     : I#DCOMRT -  LRECL = 500 BYTES                   *
      *************************************************************

         01  RT-REGISTRO.
             05 RT-COD-REG                    PIC  9(01).
             05 RT-COD-INSCR-EMPR             PIC  9(02).
             05 RT-CGC-CPF-EMPR.
                10  RT-CGC-NRO-EMPR           PIC  9(08).
                10  RT-FILIAL-EMPR            PIC  9(04).
                10  RT-CTR-EMPR               PIC  9(02).
             05 RT-NUM-INSCR-EMPR.
                10 RT-ZEROS                   PIC  9(04).
                10 RT-CART                    PIC  9(03).
                10 RT-AG-CED                  PIC  9(05).
                10 RT-CONTA-CED.
                    15 RT-NUM                 PIC  9(07).
                    15 RT-DIG                 PIC  X(01).
             05 RT-CONTR-PARTIC               PIC  X(25).
             05 RT-IDENT-TIT-BCO.
                10 RT-ZEROS                   PIC  9(08).
                10 RT-NOSSO-NUMERO.
                   15 RT-ANO                  PIC  9(02).
                   15 RT-NUMERO               PIC  9(09).
                   15 RT-DIG-NUMERO           PIC  X(01).
             05 RT-BONIF-DIA                  PIC  9(10).
             05 FILLER                        PIC  X(16).
             05 RT-COD-OCORR                  PIC  9(02).
             05 RT-SEU-NUMERO                 PIC  X(15).
             05 RT-DT-VENCTO                  PIC  9(08).
             05 RT-VLR-TIT                    PIC  9(13).
             05 RT-BANCO-DEPOS                PIC  9(03).
             05 RT-AG-DEPOS                   PIC  9(05).
             05 RT-ESPECIE                    PIC  9(02).
             05 RT-ACEITE                     PIC  9(01).
             05 RT-DT-EMISSAO                 PIC  9(08).
             05 RT-INSTRUCAO1                 PIC  9(02).
             05 RT-INSTRUCAO2                 PIC  9(02).
             05 RT-VLR-MORA-DIA               PIC  9(13).
             05 RT-DT-DESC                    PIC  9(08).
             05 RT-VLR-DESC                   PIC  9(13).
             05 RT-VLR-ISOF                   PIC  9(13).
             05 RT-ABATIM                     PIC  9(13).
             05 RT-COD-INSCR-SACADO           PIC  9(01).
             05 RT-NUM-INSCR-SACADO.
                10  RT-CGC-NRO-SACADO         PIC  9(09).
                10  RT-FILIAL-SACADO          PIC  9(04).
                10  RT-CTR-SACADO             PIC  9(02).
             05 RT-NOME-SACADO                PIC  X(40).
             05 RT-ENDERECO-SACADO            PIC  X(40).
             05 RT-PRIMEIRA-MENS              PIC  X(12).
             05 RT-CEP-NOVO.
                10 RT-CEP                     PIC  9(05).
                10 RT-SUFIXO                  PIC  X(03).
             05 RT-SEGUNDA-MENS               PIC  X(60).
             05 RT-NUM-SEQ                    PIC  9(06).
             05 FILLER                        PIC  X(09).

      *--->  CAMPOS ADICIONAIS DO DCOM

             05 RT-DANO-OPER-DESC             PIC S9(04)V  USAGE COMP-3.
             05 RT-NSEQ-OPER-DESC             PIC S9(09)V  USAGE COMP-3.
             05 RT-CTITLO-COBR-BCO            PIC S9(18)V  USAGE COMP-3.
             05 RT-AGENCIA                    PIC  9(05).
             05 RT-CONTA                      PIC  9(07).
             05 RT-DIGITO                     PIC  X(01).
             05 RT-DT-GRAVACAO                PIC  9(08).
             05 RT-NUM-AUT                    PIC  9(04).
             05 RT-CPRODT                     PIC S9(03)V  USAGE COMP-3.
             05 RT-CSPROD-DESC-COML           PIC S9(03)V  USAGE COMP-3.
             05 RT-CTPO-DESC-COML             PIC S9(03)V  USAGE COMP-3.
             05 RT-CMEIO-ENTRD-DESC           PIC S9(03)V        COMP-3.
             05 RT-CINDCD-PROTE-TITLO         PIC  X(01).
             05 RT-QDIA-PROTE-TITLO           PIC S9(03)V  USAGE COMP-3.
             05 RT-NUM-EMPRESA                PIC  9(07).
             05 RT-CART-ANT                   PIC  9(03).
             05 RT-ESPE-ANT                   PIC  9(02).
             05 RT-FX-VCTO-TIT                PIC  9(02).
             05 RT-BANCO-MIGRADO              PIC  X(01).
             05 RT-NRO-CONTR-LIM              PIC  9(11).
