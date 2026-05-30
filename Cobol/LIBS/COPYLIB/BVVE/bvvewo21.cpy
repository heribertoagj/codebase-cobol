      *----------------------------------------------------------------*
      *            COMMAREA PARA PROGRAMA BVVEWO21                     *
      *            COMUNICACAO COM A CIP - MENSAGEM RRC0021            *
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
      *                      A L T E R A C A O                         *
      *----------------------------------------------------------------*
      *    PROGRAMADOR.: TIAGO MACEDO - 7COMM                          *
      *    DATA........: 16/01/2021                                    *
      *    OBJETIVO....: DE:   BVVEWO21-VLRNEGCD-X  USAGE COMP-2.      *
      *                  PARA: BVVEWO21-VLRNEGCD-X  USAGE COMP-1.      *
      *----------------------------------------------------------------*
      *
            05     BVVEWO21-MAPA REDEFINES BVVEWR00-MENSAGEM.
             07    FILLER OCCURS 50 TIMES.
      *            CNPJ (COMPLETO OU BASE) OU CPF DO TITULAR
              10   BVVEWO21-CNPJCPFTITLAR                   PIC X(014).
      *            CNPJ OU CPF TITULAR CONTA
              10   BVVEWO21-CNPJOUCPFTITLARCT               PIC X(014).
      *            ISPB BANCO RECEBEDOR
              10   BVVEWO21-ISPBBCORECBDR                   PIC X(008).
      *            TIPO CONTA
              10   BVVEWO21-TPCT                            PIC X(002).
      *            AGÍNCIA
**************10***BVVEWO21-AG-N                            PIC*9(002)
******************************************************************COMP.
              10   BVVEWO21-AG                              PIC 9(004).
      *            CONTA
**************10***BVVEWO21-CT-N****************************PIC*9(002)
******************************************************************COMP.
              10   BVVEWO21-CT                              PIC 9(013).
*******************CONTA PAGAMENTO
**************10***BVVEWO21-CTPGTO-N                        PIC*9(002)
*******************                                             **COMP.
**************10***BVVEWO21-CTPGTO                          PIC*9(021).
      *            PRIORIDADE NEGOCIAÁ„O RECEBÌVEL
              10   BVVEWO21-PRIORDDNEGCRECBVL               PIC 9(009).
      *            CNPJ CREDENCIADORA/SUBCREDENCIADORA
              10   BVVEWO21-CNPJCREDDRSUB                   PIC X(014).
      *            CNPJ OU CPF USU·RIO FINAL RECEBEDOR
              10   BVVEWO21-CNPJCPFUSUFINRECBDR             PIC X(014).
      *            CÛDIGO INSTITUIDOR ARRANJO PAGAMENTO
              10   BVVEWO21-CODINSTITDRARRAJPGTO            PIC X(003).
      *            DATA PREVISTA LIQUIDAÁ„O
              10   BVVEWO21-DTPREVTLIQUID                   PIC X(010).
      *            VALOR NEGOCIADO
              10   BVVEWO21-VLRNEGCD-D    PIC 9(009) COMP-4.
              10   BVVEWO21-VLRNEGCD-X    PIC X(004).
              10   BVVEWO21-VLRNEGCD-9  REDEFINES
                   BVVEWO21-VLRNEGCD-X  USAGE COMP-1.
