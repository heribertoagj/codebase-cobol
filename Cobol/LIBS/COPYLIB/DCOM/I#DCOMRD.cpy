      ******************************************************************
      * SISTEMA : DCOM - I#DCOMRD                                      *
BSI001* TAMANHO : 0600 BYTES                                           *
      * ARQUIVO : PARCELA OPERACAO DESCONTO COMERCIAL COM DADOS DO     *
      *           CONTA CORRENTE ,  VALOR DA DIFERENCA E DADOS PARA    *
      *           REPOSICAO NO CONE, CONVENIO E CONTRATO LIMITE        *
      *           TABELA DCOMB0C9 - DB2PRD.TPCELA_OPER_DESC            *
      ******************************************************************
BSI001*                    ALTERACAO - BSI TECNOLOGIA                  *
BSI001*----------------------------------------------------------------*
BSI001*  ANALISTA BSI:   FERNANDO LUIZ DE SANTI  -  BSI TECNOLOGIA     *
BSI001*  DATA........:   28/04/2017                                    *
BSI001*----------------------------------------------------------------*
BSI001*  OBJETIVO....:   INCLUIR CAMPOS DE ENCARGOS MORATORIOS.        *
BSI001******************************************************************
       01  RD-REGISTRO.
  1  3     05 RD-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
  4  5     05 RD-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
  9  3     05 RD-NPCELA-DESC-COML     PIC S9(5)V USAGE COMP-3.
 12  2     05 RD-DMES-VCTO-PTCAO      PIC S9(2)V USAGE COMP-3.
 14  2     05 RD-CSIT-DESC-COML       PIC S9(3)V USAGE COMP-3.
 16 26     05 RD-HSIT-DESC-COML       PIC X(26).
 42 10     05 RD-DVCTO-PCELA-DESC     PIC X(10).
 52  9     05 RD-VPRINC-PCELA-DESC    PIC S9(15)V9(2) USAGE COMP-3.
 61  5     05 RD-QTITLO-CHEQ-PCELA    PIC S9(9)V USAGE COMP-3.
 66  5     05 RD-PTX-JURO-MES         PIC S9(3)V9(5) USAGE COMP-3.
 71  5     05 RD-PTX-JURO-ANO         PIC S9(3)V9(5) USAGE COMP-3.
 76  5     05 RD-PTX-CUSTO-MES        PIC S9(3)V9(6) USAGE COMP-3.
 81  5     05 RD-PTX-CUSTO-ANO        PIC S9(3)V9(6) USAGE COMP-3.
 86  9     05 RD-VCUSTO-PCELA-DESC    PIC S9(15)V9(2) USAGE COMP-3.
 95  9     05 RD-VJURO-PCELA-DESC     PIC S9(15)V9(2) USAGE COMP-3.
104  9     05 RD-VJURO-DIA-PCELA      PIC S9(15)V9(2) USAGE COMP-3.
113  9     05 RD-VIOF-PCELA-DESC      PIC S9(15)V9(2) USAGE COMP-3.
122  9     05 RD-VPSENT-PCELA-DESC    PIC S9(15)V9(2) USAGE COMP-3.
131  9     05 RD-VJURO-APROP-ACUM     PIC S9(15)V9(2) USAGE COMP-3.
140 10     05 RD-DULT-APROP-PCELA     PIC X(10).
150  5     05 RD-PALIQT-IOF-PRROG     PIC S9(3)V9(5) USAGE COMP-3.
155  9     05 RD-VIOF-PRROG-PCELA     PIC S9(15)V9(2) USAGE COMP-3.
164  5     05 RD-PCUSTO-MES-MORA      PIC S9(3)V9(6) USAGE COMP-3.
169  5     05 RD-PCUSTO-ANO-MORA      PIC S9(3)V9(6) USAGE COMP-3.
174  5     05 RD-PCUSTO-MES-CRATS     PIC S9(3)V9(6) USAGE COMP-3.
179  5     05 RD-PCUSTO-ANO-CRATS     PIC S9(3)V9(6) USAGE COMP-3.
184  5     05 RD-PALIQT-IOF-VENCD     PIC S9(3)V9(5) USAGE COMP-3.
189  9     05 RD-VIOF-PCELA-VENCD     PIC S9(15)V9(2) USAGE COMP-3.
198  9     05 RD-VJURO-MORA-GERC      PIC S9(15)V9(2) USAGE COMP-3.
207  9     05 RD-VJURO-MORA-LEGAL     PIC S9(15)V9(2) USAGE COMP-3.
216  9     05 RD-VSDO-DVDOR-PRINC     PIC S9(15)V9(2) USAGE COMP-3.
225  9     05 RD-VCUSTO-PCELA-VENCD   PIC S9(15)V9(2) USAGE COMP-3.
234  9     05 RD-VJURO-TRNSF-CRATS    PIC S9(15)V9(2) USAGE COMP-3.
243  9     05 RD-VCUSTO-TRNSF-CRATS   PIC S9(15)V9(2) USAGE COMP-3.
252 10     05 RD-DTRNSF-CRATS-PCELA   PIC X(10).
262 10     05 RD-DTRNSF-LP-PCELA      PIC X(10).
272  5     05 RD-CBACEN-DESC-RURAL    PIC S9(9)V USAGE COMP-3.
277  4     05 RD-CCONTR-CORP-PCELA    PIC S9(7)V USAGE COMP-3.
281 10     05 RD-DULT-ADTTO-PCELA     PIC X(10).
291  1     05 RD-CINDCD-MOTVO-ADTTO   PIC X(1).
292  1     05 RD-CINDCD-PEND-EXTER    PIC X(1).
293  1     05 RD-CINDCD-BLOQ-CONS     PIC X(1).
294 10     05 RD-DBLOQ-CONS-CCORR     PIC X(10).
304 10     05 RD-DULT-PRROG-PCELA     PIC X(10).
314 10     05 RD-DULT-REATV-PCELA     PIC X(10).
324 26     05 RD-HULT-ATULZ           PIC X(26).
350  5     05 RD-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
355  8     05 RD-CTERM                PIC X(8).
363 10     05 RD-DINCL-REG            PIC X(10).
373 09     05 RD-VJURO-REMUN-VENCD    PIC S9(15)V9(2) USAGE COMP-3.
382 09     05 RD-VMORA-VENCD-DESC     PIC S9(15)V9(2) USAGE COMP-3.
391 09     05 RD-VMULTA-VENCD-DESC    PIC S9(15)V9(2) USAGE COMP-3.
400 25     05 RD-FILLER-02            PIC  X(25).
      **                                                              **
      **----- DADOS DO CONTA CORRENTE                              ---**
      **                                                              **
425  1     05 RD-PGTO-CTACORR         PIC  X(1).
      **       S -  PAGO NO CONTA CORRENTE                            **
      **       N -  PAGO NA COBRANCA                                  **
      **       A -  AMBOS (PAGO NA COBRANCA E NO CONTA CORRENTE)      **
426  1     05 RD-PGTO-CHEQUE-MOD      PIC  X(01).
      **       S -  PAGAMENTO COM CHEQUE
      **       N -  PAGAMENTOS DIFERENTES DE CHEQUE
427  1     05 RD-PGTO-TOTAL-PARCIAL   PIC  X(01).
      **       T -  PAGTO TOTAL
      **       P -  PAGTO PARCIAL
428  2     05 RD-COD-BAIXA            PIC  9(3)     COMP-3.
           05 RD-VALOR-ENVIADO-CTA-CORR.
430  8        07  RD-VLR-TOTAL        PIC 9(013)V99 COMP-3.
438  8        07  RD-VLR-PRINCIPAL    PIC 9(013)V99 COMP-3.
446  8        07  RD-VLR-ENCARGOS     PIC 9(013)V99 COMP-3.
454  8        07  RD-VLR-IOF          PIC 9(013)V99 COMP-3.
           05 RD-VALOR-DEBITADO-CTA-CORR.
462  8        07  RD-TOTAL-DEB        PIC  9(13)V99 COMP-3.
470  8        07  RD-PRINCIPAL-DEB    PIC  9(13)V99 COMP-3.
478  8        07  RD-ENCARGOS-DEB     PIC  9(13)V99 COMP-3.
486  8        07  RD-IOF-DEB          PIC  9(13)V99 COMP-3.
494  2     05 RD-CBCO-CHEQ-MOD        PIC S9(3)V      USAGE COMP-3.
496  3     05 RD-CAG-BCRIA-CHEQ-MOD   PIC S9(5)V      USAGE COMP-3.
499  6     05 RD-CCTA-CORR-CHEQ-MOD   PIC S9(11)V     USAGE COMP-3.
505  4     05 RD-CCHEQ-MOD            PIC S9(7)V      USAGE COMP-3.
      **                                                              **
      **----- VALOR DA DIFERENCA PAGA NO CONTA CORRENTE E COBRANCA ---**
      **                                                              **
509  8     05 RD-VR-DIFERENCA         PIC S9(13)V9(2) USAGE COMP-3.
      **                                                              **
      **----- DADOS PARA REPOSICAO DO CONE, CONVENIO E CONTRATO    ---**
      **                                                              **
517  5     05 RD-CCONTR-CONVE-DESC    PIC S9(9)V USAGE COMP-3.
522  2     05 RD-CVRSAO-CONTR-CONVE   PIC S9(3)V USAGE COMP-3.
524  2     05 RD-CELMTO-DESC-COML     PIC S9(3)V USAGE COMP-3.
526  3     05 RD-DANO-BASE            PIC S9(4)V USAGE COMP-3.
529  3     05 RD-CDEPDC               PIC S9(5)V USAGE COMP-3.
532  5     05 RD-CSEQ-STUDO           PIC S9(9)V USAGE COMP-3.
537  5     05 RD-CCART-CONE           PIC X(5).
542  5     05 RD-CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
547  3     05 RD-CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
550  2     05 RD-CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
552  8     05 RD-VR-PAGO-PARC         PIC S9(13)V9(2) USAGE COMP-3.
      **                                                              **
      **----- VALOR PAGO NA COBRANCA
560  8     05 RD-VLR-PAGO-COBR        PIC S9(13)V9(2) USAGE COMP-3.
      **                                                              **
      *---    S = PAGAMENTO VINCULADO NA COBRANCA (CHEQUE)
568  1     05 RD-PAGO-COBR-CHEQ-VINC  PIC  X(01).
      **                                                              **
      *---    S = PAGAMENTO VINCULADO DEVOLVIDO PELA COBRANCA
569  1     05 RD-CHEQ-VINC-DEVO-COBR  PIC  X(01).
      **                                                              **
      *---    AGENCIA/CONTA DO CEDENTE/EMITENTE DA OPERACAO
      *---   (EMITENTE APENAS NA EMISSAO DO RELATORIO DE RETORNO)
570  3     05 RD-CAG-BCRIA            PIC S9(05) COMP-3.
573  7     05 RD-CCTA-BCRIA-CLI       PIC S9(13) COMP-3.
      **                                                              **
      *---    CONTRATO CORPORATIVO DO CHEQUE
580  4     05 RD-CCONTR-CORP-CHEQC    PIC S9(07) COMP-3.
584 17     05 RD-FILLER               PIC  X(17).
